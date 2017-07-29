package com.github.symcal

import scala.language.implicitConversions

sealed trait Expr {
  def +(x: Expr): Add = Add(this, x)

  def -(x: Expr): Subtract = Subtract(this, x)

  def unary_- : Expr = this match {
    case Minus(x) ⇒ x
    case y ⇒ Minus(y)
  }

  def *(x: Expr): Multiply = Multiply(this, x)

  // The '#' character is needed for precedence
  def #^(d: Int): IntPow = IntPow(this, d)

  def toInt: Int

  final def diff(x: Var): Expr = diffInternal(x).simplify

  private[symcal] def diffInternal(x: Var): Expr

  def simplify: Expr

  def subs(v: Var, e: Expr): Expr

  final private[symcal] def stringForm(level: Int): String =
    if (precedenceLevel < level)
      "(" + printInternal + ")"
    else
      printInternal

  def precedenceLevel: Int

  protected def printInternal: String

  final def print: String = stringForm(0)

  def freeVars: Set[Var] = Expr.freeVars(this)

  def isConst: Boolean = false

  def expand: Expr = expandInternal.simplify

  private[symcal] def expandInternal: Sum
}

object Expr {
  /** If this implicit conversion is moved to `package.scala`, the expression 4 + Var('x) does not compile.
    */
  implicit def intToConst(x: Int): Const = {
    Const(x)
  }

  final val precedenceOfAdd = 20
  final val precedenceOfSubtract = 30
  final val precedenceOfMinus = 40
  final val precedenceOfMultiply = 50
  final val precedenceOfIntPow = 60
  final val precedenceOfConst = 100

  def freeVars(e: Expr): Set[Var] = e match {
    case Const(value) ⇒ Set()
    case Subtract(x, y) ⇒ freeVars(x) ++ freeVars(y)
    case Minus(x) ⇒ freeVars(x)
    case Add(x, y) ⇒ freeVars(x) ++ freeVars(y)
    case Multiply(x, y) ⇒ freeVars(x) ++ freeVars(y)
    case Var(name) ⇒ Set(Var(name))
    case IntPow(x, d) ⇒ freeVars(x)
    case Sum(es@_*) ⇒ es.flatMap(freeVars).toSet
    case Product(es@_*) ⇒ es.flatMap(freeVars).toSet
  }

  /** Auxiliary function. Returns the sequence of coefficients and powers for multinomial expansion.
    * For example, to compute the expansion `(x + y)^3` we call `getTermCoeffs(2, 3)`, which returns the sequence
    * {{{
    *   Seq(
    *     (1, Seq(3, 0)),
    *     (3, Seq(2, 1)),
    *     (3, Seq(1, 2)),
    *     (1, Seq(0, 3))
    *   )
    * }}}
    * This allows us to build the expansion as `x^3 + 3*x^2*y + 3*x*y^2 + y^3`.
    *
    * @param len   Length of the sum list to be expanded.
    * @param power Power of the expansion.
    * @return Sequence of coefficients and power indices for individual subterms.
    */
  private[symcal] def getTermCoeffs(len: Int, power: Int): Seq[(Int, Seq[Int])] = {

    /** Auxiliary function. Returns the sequence of combination numbers, together with the sequence of indices.
      * For example, `getCombinationNumbers(4)` returns `Seq( (4, 1), (3, 4), (2, 6), (1, 4), (0, 1) )`
      * which corresponds to the coefficients in the expansion `(x + 1) ^ 4 = x^4 + 4*x^3 + 6*x^2 + 4*x + 1`.
      *
      * @param total The number of elements.
      * @return Sequence of pairs `(i, c)` where `i` goes from `total` to `0` and
      *         `c` is equal to the number of combinations of `i` from `total`.
      *         `c == total! / (i! * (total - i)! )`
      */
    def getCombinationNumbers(total: Int): Seq[(Int, Int)] = {
      val ordering = total to 0 by -1
      ordering zip ordering.scanLeft(1) { case (c, i) ⇒ c * i / (total - i + 1) }
    }

    def getTermCoeffsRec(m: Int, total: Int): Seq[(Int, List[Int])] = {
      if (m <= 1 || total == 0) {
        Seq((1, List.fill[Int](m)(total)))
      } else {
        val result: Seq[(Int, List[Int])] = for {
          pC <- getCombinationNumbers(total)
          (p, c) = pC
          termCoeffs <- getTermCoeffsRec(m - 1, total - p)
          (coeff, indices) = termCoeffs
        } yield
          (c * coeff, p :: indices)
        result
      }
    }

    getTermCoeffsRec(len, power)
  }
}

final case class Const(value: Int) extends Expr {
  override def toInt: Int = value

  private[symcal] def diffInternal(x: Var): Expr = Const(0)

  override def subs(v: Var, e: Expr): Expr = this

  override def precedenceLevel: Int = Expr.precedenceOfConst

  override def printInternal: String = value.toString

  override def simplify: Expr = this

  override def isConst: Boolean = true

  override def expandInternal: Sum = Sum(this)
}

final case class Subtract(x: Expr, y: Expr) extends Expr {
  override def toInt: Int = x.toInt - y.toInt

  private[symcal] def diffInternal(z: Var): Expr = x.diff(z) - y.diff(z)

  override def subs(v: Var, e: Expr): Expr = (x.subs(v, e) - y.subs(v, e)).simplify

  override def precedenceLevel: Int = Expr.precedenceOfSubtract

  override def simplify: Expr = (x.simplify, y.simplify) match {
    case (Const(0), ys) ⇒ Minus(ys).simplify
    case (xs, Const(0)) ⇒ xs
    case (Const(a), Const(b)) ⇒ Const(a - b)
    case (xs, ys) ⇒ xs - ys
  }

  override protected def printInternal: String = x.stringForm(Expr.precedenceOfAdd) + " - " + y.stringForm(Expr.precedenceOfMultiply)

  /** The result of `expand` is always a [[Sum]] that contains no nested [[Sum]]s.
    *
    * @return Equivalent expression with flattened structure, expanding all parentheses.
    */
  override def expandInternal: Sum = Sum(x.expandInternal.es ++ y.expandInternal.es.map(-_): _*)
}

final case class Minus(x: Expr) extends Expr {
  override def toInt: Int = -x.toInt

  private[symcal] def diffInternal(z: Var): Expr = -x.diff(z)

  override def subs(v: Var, e: Expr): Expr = (-x.subs(v, e)).simplify

  override def precedenceLevel: Int = Expr.precedenceOfMinus

  override protected def printInternal: String = "-" + x.stringForm(precedenceLevel)

  override def simplify: Expr = x.simplify match {
    case Const(a) ⇒ Const(-a)
    case Minus(a) ⇒ a
    case xs => Minus(xs)
  }

  override def expandInternal: Sum = Sum(x.expandInternal.es.map(-_): _*)
}

final case class Add(x: Expr, y: Expr) extends Expr {
  override def toInt: Int = x.toInt + y.toInt

  private[symcal] def diffInternal(z: Var): Expr = x.diff(z) + y.diff(z)

  override def simplify: Expr = (x.simplify, y.simplify) match {
    case (Const(0), ys) => ys
    case (xs, Const(0)) => xs
    case (Const(a), Const(b)) => Const(a + b)
    case (xs, ys) => xs + ys
  }

  override def subs(v: Var, e: Expr): Expr = (x.subs(v, e) + y.subs(v, e)).simplify

  override def printInternal: String = {
    val rest = y match {
      case Minus(yy) ⇒ " - " + yy.stringForm(Expr.precedenceOfMultiply)
      case _ ⇒ " + " + y.stringForm(precedenceLevel)
    }
    x.stringForm(precedenceLevel) + rest
  }

  override def precedenceLevel: Int = Expr.precedenceOfAdd

  override def expandInternal: Sum = Sum(x.expandInternal.es ++ y.expandInternal.es: _*)
}

final case class Multiply(x: Expr, y: Expr) extends Expr {
  override def toInt: Int = x.toInt * y.toInt

  private[symcal] def diffInternal(z: Var): Expr = Multiply(x.diff(z), y) + Multiply(x, y.diff(z))

  override def simplify: Expr = (x.simplify, y.simplify) match {
    case (Const(1), ys) => ys
    case (xs, Const(1)) => xs
    case (Const(0), _) => Const(0)
    case (_, Const(0)) => Const(0)
    case (Const(a), Const(b)) => Const(a * b)
    case (xs, ys) => xs * ys
  }

  override def subs(v: Var, e: Expr): Expr = (x.subs(v, e) * y.subs(v, e)).simplify

  override def printInternal: String = x.stringForm(precedenceLevel) + " * " + y.stringForm(precedenceLevel)

  override def precedenceLevel: Int = Expr.precedenceOfMultiply

  override def expandInternal: Sum = {
    val terms = for {
      xx <- x.expandInternal.es
      yy <- y.expandInternal.es
    } yield Product(xx, yy).flatten
    Sum(terms: _*)
  }
}

final case class Var(name: Symbol) extends Expr {
  override def toInt: Int =
    throw new Exception(s"Cannot evaluate toInt for an expression containing a variable ${name.name}.")

  private[symcal] def diffInternal(x: Var): Expr = if (name == x.name) Const(1) else Const(0)

  override def subs(v: Var, e: Expr): Expr = v match {
    case Var(`name`) ⇒ e
    case _ ⇒ this
  }

  override def printInternal: String = name.name

  override def precedenceLevel: Int = Expr.precedenceOfConst

  override def simplify: Expr = this

  override def expandInternal: Sum = Sum(this)
}

final case class IntPow(x: Expr, d: Const) extends Expr {
  override def toInt: Int = Math.pow(x.toInt, d.value).toInt

  private[symcal] def diffInternal(z: Var): Expr = d match {
    case Const(0) => Const(0)
    case Const(1) => x.diffInternal(z) // no need to `diff` here because `simplify` will follow
    case _ => d * x.diff(z) * IntPow(x, Const(d.value - 1))
  }

  override def simplify: Expr = (x.simplify, d) match {
    case (Const(a), _) => Const(IntPow(Const(a), d).toInt)
    case (xs, Const(1)) => xs
    case (_, Const(0)) => Const(1)
    case (xs, _) ⇒ IntPow(xs, d)
  }

  override def subs(v: Var, e: Expr): Expr = IntPow(x.subs(v, e), d).simplify

  override def printInternal: String = x.stringForm(precedenceLevel + 1) + "^" + d.print

  override def precedenceLevel: Int = Expr.precedenceOfIntPow

  override def expandInternal: Sum =
    if (d.toInt >= 0) {
      // We can expand only if the exponent is non-negative.
      val xs = x.expandInternal.es
      (xs.headOption, xs.drop(1)) match {
        case (None, _) ⇒ Sum(IntPow(Const(0), d)) // Empty Sum is equivalent to 0.
        case (Some(head), tail) if tail.isEmpty ⇒ // If x.expand has only one term, we have nothing to expand.
          Sum(IntPow(head, d))
        case _ ⇒ // x.expand has at least 2 terms, need to expand
          val newMonomials = Expr.getTermCoeffs(xs.length, d.toInt) map {
            case (coeff, powers) ⇒
              val newTerms = (xs zip powers).map { case (e, i) ⇒ IntPow(e, i) } :+ Const(coeff)
              Product(newTerms: _*)
          }
          Sum(newMonomials: _*)
      }
    } else {
      // Cannot expand a negative power.
      Sum(this)
    }
}

final case class Sum(es: Expr*) extends Expr {
  override def toInt: Int = es.map(_.toInt).sum

  private[symcal] def diffInternal(x: Var): Expr = Sum(es.map(_.diff(x)): _*)

  override def subs(v: Var, e: Expr): Expr = Sum(es.map(_.subs(v, e)): _*)

  override def precedenceLevel: Int = Expr.precedenceOfAdd

  override protected def printInternal: String = (es.headOption, es.drop(1)) match {
    case (None, _) => "0" // empty Sum()
    case (Some(head), tail) ⇒ head.stringForm(precedenceLevel) +
      tail.map {
        case Minus(t) ⇒ " - " + t.stringForm(Expr.precedenceOfMultiply)
        case t ⇒ " + " + t.stringForm(precedenceLevel)
      }
        .mkString("")
  }

  override def simplify: Expr = {
    val (const, nonconst) = es.map(_.simplify).partition(_.isConst)
    // mergedConstants is always non-empty but can be Const(0)
    val mergedConstants = const.foldLeft[Expr](Const(0))((x, y) ⇒ (x + y).simplify)
    val mergedExprs =
      mergedConstants match {
        case Const(0) ⇒ nonconst
        case _ ⇒ nonconst :+ mergedConstants
      }
    // There are three cases now: empty sequence, one expr, and more than one expr.
    mergedExprs.headOption match {
      case Some(e) ⇒
        if (mergedExprs.length == 1) {
          // In this case, the simplified result is not a `Sum`.
          e
        } else Sum(mergedExprs: _*)
      case None ⇒ Const(0) // Empty sum is transformed into `Const(0)`.
    }
  }

  override def expandInternal: Sum = Sum(es.flatMap(_.expandInternal.es): _*)
}

final case class Product(es: Expr*) extends Expr {
  override def toInt: Int = es.map(_.toInt).product

  override def diffInternal(x: Var): Expr = {
    val diffs = es.map(_.diff(x))
    val replaced = diffs.zipWithIndex.map { case (expr, index) ⇒ Product(es.updated(index, expr): _*) }
    Sum(replaced: _*)
  }

  override def subs(v: Var, e: Expr): Expr = Product(es.map(_.subs(v, e)): _*)

  override def precedenceLevel: Int = Expr.precedenceOfMultiply

  override protected def printInternal: String = es.map(_.stringForm(precedenceLevel)).mkString(" * ")

  override def simplify: Expr = {
    val (const, nonconst) = es.map(_.simplify).partition(_.isConst)
    // mergedConstants is always non-empty but can be Const(0) or Const(1)
    val mergedConstants = const.foldLeft[Expr](Const(1))((x, y) ⇒ (x * y).simplify)
    val mergedExprs =
      mergedConstants match {
        case Const(0) ⇒ Seq(Const(0))
        case Const(1) ⇒ nonconst
        case _ ⇒ nonconst :+ mergedConstants
      }
    // There are three cases now: empty sequence, one expr, and more than one expr.
    mergedExprs.headOption match {
      case Some(e) ⇒
        if (mergedExprs.length == 1) {
          // In this case, the simplified result is not a `Product`.
          e
        } else Product(mergedExprs: _*)
      case None ⇒ Const(1) // Empty product is transformed into `Const(1)`.
    }
  }

  override def expandInternal: Sum = (es.headOption, es.drop(1)) match {
    case (None, _) ⇒ Sum(Const(1)) // empty Product()
    case (Some(head), tail) ⇒
      val terms = for {
        t <- head.expandInternal.es
        z <- Product(tail: _*).expandInternal.es
      } yield Product(t, z).flatten
      Sum(terms: _*)
  }

  /** If any of the multiplicands are a [[Product]], the list is flattened.
    *
    * @return A simplified (flattened) but equivalent [[Product]] term.
    */
  private[symcal] def flatten: Product = {
    val newTerms = es.flatMap {
      case Product(fs@_*) ⇒ fs
      case t ⇒ Seq(t)
    }
    Product(newTerms: _*)
  }
}
