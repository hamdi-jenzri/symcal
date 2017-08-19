package com.github.symcal

import spire.algebra._
import spire.implicits._
import scala.language.implicitConversions

sealed abstract class Expr[T: Ring : Eq] {
  def +(x: Expr[T]): Sum[T] = (this, x) match {
    case (Sum(y@_*), Sum(z@_*)) => Sum(y ++ z: _*)
    case (Sum(y@_*), _) => Sum(y :+ x: _*)
    case (_, Sum(z@_*)) => Sum(Seq(this) ++ z: _*)
    case (_, _) => Sum(this, x)
  }

  def -(x: Expr[T]): Sum[T] = (this, x) match {
    case (Sum(y@_*), Sum(z@_*)) => Sum(y ++ z.map(-_): _*)
    case (Sum(y@_*), _) => Sum(y :+ -x: _*)
    case (_, Sum(z@_*)) => Sum(Seq(this) ++ z.map(-_): _*)
    case (_, _) => Sum(this, -x)
  }

  def unary_- : Expr[T] = this match {
    case Minus(x) ⇒ x
    case y ⇒ Minus(y)
  }

  def *(x: Expr[T]): Product[T] = (this, x) match {
    case (Product(y@_*), Product(z@_*)) => Product(y ++ z: _*)
    case (Product(y@_*), _) => Product(y :+ x: _*)
    case (_, Product(z@_*)) => Product(Seq(this) ++ z: _*)
    case (_, _) => Product(this, x)
  }

  // The '#' character is needed for precedence
  def #^(d: Int): IntPow[T] = IntPow(this, Const(d))

  def toValue: T

  final def diff(x: Var[T]): Expr[T] = diffInternal(x).simplify

  private[symcal] def diffInternal(x: Var[T]): Expr[T]

  def simplify: Expr[T]

  final def subs(v: Var[T], e: Expr[T]): Expr[T] = subsInternal(v, e).simplify

  private[symcal] def subsInternal(v: Var[T], e: Expr[T]): Expr[T]

  final private[symcal] def stringForm(level: Int): String =
    if (precedenceLevel < level)
      "(" + printInternal + ")"
    else
      printInternal

  def precedenceLevel: Int

  protected def printInternal: String

  final def print: String = stringForm(0)

  def freeVars: Set[Var[T]] = Expr.freeVars(this)

  def isConst: Boolean = false

  def isSum: Boolean = false

  def isProduct: Boolean = false

  def expand: Expr[T] = expandInternal.simplify

  private[symcal] def expandInternal: Sum[T]
}

object Expr {
  /** If this implicit conversion is moved to `package.scala`, the expression 4 + Var('x) does not compile.
    */
  implicit def valueToConst[T: Ring : Eq](x: T): Const[T] = Const(x)

  final val precedenceOfAdd = 20
  final val precedenceOfSubtract = 30
  final val precedenceOfMinus = 40
  final val precedenceOfMultiply = 50
  final val precedenceOfIntPow = 60
  final val precedenceOfConst = 100

  def freeVars[T: Ring : Eq](e: Expr[T]): Set[Var[T]] = e match {
    case Const(_) ⇒ Set()
    case Minus(x) ⇒ freeVars(x)
    case Var(name) ⇒ Set(Var(name))
    case IntPow(x, _) ⇒ freeVars(x)
    case Sum(es@_*) ⇒ es.flatMap(e ⇒ freeVars(e)).toSet
    case Product(es@_*) ⇒ es.flatMap(e ⇒ freeVars(e)).toSet
  }

  def constZero[T: Ring : Eq] = Const(implicitly[Ring[T]].zero)

  def constOne[T: Ring : Eq] = Const(implicitly[Ring[T]].one)

  def intToConst[T: Ring : Eq](x: Int): Const[T] = Const(implicitly[Ring[T]].fromInt(x))

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

final case class Const[T: Ring : Eq](value: T) extends Expr[T] {
  override def toValue: T = value

  private[symcal] def diffInternal(x: Var[T]): Expr[T] = Expr.constZero[T]

  override def subsInternal(v: Var[T], e: Expr[T]): Expr[T] = this

  override def precedenceLevel: Int = Expr.precedenceOfConst

  override def printInternal: String = value.toString

  override def simplify: Expr[T] = this

  override def isConst: Boolean = true

  override def expandInternal: Sum[T] = Sum(this)
}

final case class Minus[T: Ring : Eq](x: Expr[T]) extends Expr[T] {
  override def toValue: T = -x.toValue

  private[symcal] def diffInternal(z: Var[T]): Expr[T] = -x.diff(z)

  override def subsInternal(v: Var[T], e: Expr[T]): Expr[T] = (-x.subsInternal(v, e)).simplify

  override def precedenceLevel: Int = Expr.precedenceOfMinus

  override protected def printInternal: String = "-" + x.stringForm(precedenceLevel)

  override def simplify: Expr[T] = x.simplify match {
    case Const(a) ⇒ Const(-a)
    case Minus(a) ⇒ a
    case xs => Minus(xs)
  }

  override def expandInternal: Sum[T] = Sum(x.expandInternal.es.map(-_): _*)
}

final case class Var[T: Ring : Eq](name: Symbol) extends Expr[T] {
  override def toValue: T =
    throw new Exception(s"Cannot evaluate toValue for an expression containing a variable ${name.name}.")

  private[symcal] def diffInternal(x: Var[T]): Expr[T] = {
    if (name == x.name) Expr.constOne[T] else Expr.constZero[T]
  }

  override def subsInternal(v: Var[T], e: Expr[T]): Expr[T] = v match {
    case Var(`name`) ⇒ e
    case _ ⇒ this
  }

  override def printInternal: String = name.name

  override def precedenceLevel: Int = Expr.precedenceOfConst

  override def simplify: Expr[T] = this

  override def expandInternal: Sum[T] = Sum(this)
}

final case class IntPow[T: Ring : Eq](x: Expr[T], d: Const[Int]) extends Expr[T] {
  override def toValue: T = implicitly[Ring[T]].pow(x.toValue, d.value)

  private[symcal] def diffInternal(z: Var[T]): Expr[T] = d match {
    case Const(0) => Expr.constZero[T]
    case Const(1) => x.diffInternal(z) // no need to `diff` here because `simplify` will follow
    case _ => Expr.intToConst[T](d.value) * x.diff(z) * IntPow(x, Const(d.value - 1))
  }

  override def simplify: Expr[T] = (x.simplify, d) match {
    case (Const(a), _) => Const(IntPow(Const(a), d).toValue)
    case (xs, Const(1)) => xs
    case (_, Const(0)) => Expr.constOne[T]
    case (xs, _) ⇒ IntPow(xs, d)
  }

  override def subsInternal(v: Var[T], e: Expr[T]): Expr[T] = IntPow(x.subsInternal(v, e), d).simplify

  override def printInternal: String = x.stringForm(precedenceLevel + 1) + "^" + d.print

  override def precedenceLevel: Int = Expr.precedenceOfIntPow

  override def expandInternal: Sum[T] =
    if (d.toValue >= 0) {
      // We can expand only if the exponent is non-negative.
      val xs = x.expandInternal.es
      (xs.headOption, xs.drop(1)) match {
        case (None, _) ⇒ Sum(IntPow(Expr.constZero[T], d)) // Empty Sum is equivalent to 0.
        case (Some(head), tail) if tail.isEmpty ⇒ // If x.expand has only one term, we have nothing to expand.
          Sum(IntPow(head, d))
        case _ ⇒ // x.expand has at least 2 terms, need to expand
          val newMonomials = Expr.getTermCoeffs(xs.length, d.toValue) map {
            case (coeff, powers) ⇒
              val newTerms = (xs zip powers).map { case (e, i) ⇒ IntPow(e, Const(i)) } :+ Expr.intToConst[T](coeff)
              Product(newTerms: _*)
          }
          Sum(newMonomials: _*)
      }
    } else {
      // Cannot expand a negative power.
      Sum(this)
    }
}

final case class Sum[T: Ring : Eq](es: Expr[T]*) extends Expr[T] {
  override def isSum: Boolean = true

  override def toValue: T = es.map(_.toValue).reduceOption(_ + _).getOrElse(implicitly[Ring[T]].zero)

  private[symcal] def diffInternal(x: Var[T]): Expr[T] = Sum(es.map(_.diff(x)): _*)

  override def subsInternal(v: Var[T], e: Expr[T]): Expr[T] = Sum(es.map(_.subsInternal(v, e)): _*)

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

  override def simplify: Expr[T] = {
    val (constants, nonconstants) = es.map(_.simplify).partition(_.isConst)
    val nonconstantsFlattened: Seq[Expr[T]] = nonconstants.flatMap {
      case Sum(es@_*) ⇒ es
      case x => Seq(x) // not a sum
    }
    // mergedConstants is the sum of all constants in the list; also could be 0.
    val mergedConstants: T = constants
      .collect { case x@Const(_) ⇒ x.value } // This converts into Seq[Int]. We know that we are not losing any values here.
      .reduceOption(_ + _) // This may yield `None` if sequence is empty.
      .getOrElse(implicitly[Ring[T]].zero) // An empty sequence of `constants` will produce 0 here.

    val mergedExprs: Seq[Expr[T]] = if (mergedConstants.isZero)
      nonconstantsFlattened
    else
      nonconstantsFlattened :+ Const(mergedConstants) // Constant should be last in `Sum`, e.g. `x + y + 2`.

    // There are three cases now: empty sequence, one expr, and more than one expr.
    mergedExprs.headOption match {
      case Some(e) ⇒
        if (mergedExprs.length == 1) {
          // In this case, the simplified result is a `Sum` of just one expression, so should not be a `Sum`.
          e
        } else Sum(mergedExprs: _*)
      case None ⇒ Expr.constZero[T] // Empty `Sum` is transformed into zero.
    }
  }

  override def expandInternal: Sum[T] = Sum(es.flatMap(_.expandInternal.es): _*)
}

final case class Product[T: Ring : Eq](es: Expr[T]*) extends Expr[T] {
  override def isProduct: Boolean = true

  override def toValue: T = es.map(_.toValue).reduceOption(_ * _).getOrElse(implicitly[Ring[T]].one)

  override def diffInternal(x: Var[T]): Expr[T] = {
    val diffs = es.map(_.diff(x))
    val replaced = diffs.zipWithIndex.map { case (expr, index) ⇒ Product(es.updated(index, expr): _*) }
    Sum(replaced: _*)
  }

  override def subsInternal(v: Var[T], e: Expr[T]): Expr[T] = Product(es.map(_.subsInternal(v, e)): _*)

  override def precedenceLevel: Int = Expr.precedenceOfMultiply

  override protected def printInternal: String = es.map(_.stringForm(precedenceLevel)).mkString(" * ")

  override def simplify: Expr[T] = {
    val (constants, nonconstants) = es.map(_.simplify).partition(_.isConst)
    val nonconstantsFlattened: Seq[Expr[T]] = nonconstants.flatMap {
      case Product(es@_*) ⇒ es
      case x => Seq(x) // not a product
    }
    // mergedConstants is the product of all constants in the list; also could be 0 or 1.
    val mergedConstants: T = constants
      .collect { case x@Const(_) ⇒ x.value } // This converts into Seq[Int]. We know that we are not losing any values here.
      .reduceOption(_ * _) // This may yield `None` if sequence is empty.
      .getOrElse(implicitly[Ring[T]].one) // An empty sequence of `constants` will produce 1 here.

    val mergedExprs: Seq[Expr[T]] = if (mergedConstants.isZero)
      Seq(Expr.constZero[T])
    else if (mergedConstants.isOne)
      nonconstantsFlattened
    else Seq(Const(mergedConstants)) ++ nonconstantsFlattened // Constant should be first in `Product`, e.g. `2 * x * y`.

    // There are three cases now: empty sequence, one expr, and more than one expr.
    mergedExprs.headOption match {
      case Some(e) ⇒
        if (mergedExprs.length == 1) {
          // In this case, the simplified result is a `Product` of just one expression, so should not be a `Product`.
          e
        } else Product(mergedExprs: _*)
      case None ⇒ Expr.constOne[T] // Empty `Product` is transformed into 1.
    }
  }

  override def expandInternal: Sum[T] = (es.headOption, es.drop(1)) match {
    case (None, _) ⇒ Sum(Expr.constOne[T]) // empty Product()
    case (Some(head), tail) ⇒ // Product(head, ....tail....)
      val terms = for {
        t <- head.expandInternal.es
        z <- Product(tail: _*).expandInternal.es
      } yield Product(t, z).flatten
      Sum(terms: _*)
  }

  /** If any of the multiplicands are a [[Product]], the list is flattened.
    * This auxiliary function is used to expand a [[Product]] that may contain nested [[Sum]] and [[Product]] expressions.
    *
    * @return A simplified (flattened) but equivalent [[Product]] term.
    */
  private[symcal] def flatten: Product[T] = {
    val newTerms = es.flatMap {
      case Product(fs@_*) ⇒ fs
      case t ⇒ Seq(t)
    }
    Product(newTerms: _*)
  }
}
