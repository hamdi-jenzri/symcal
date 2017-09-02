package com.github.symcal

import spire.algebra._
import spire.implicits._

import scala.language.implicitConversions

sealed abstract class Expr[T: Ring : Eq] {
  final def +(x: Expr[T]): Sum[T] = (this, x) match {
    case (Sum(y@_*), Sum(z@_*)) => Sum(y ++ z: _*)
    case (Sum(y@_*), _) => Sum(y :+ x: _*)
    case (_, Sum(z@_*)) => Sum(Seq(this) ++ z: _*)
    case (_, _) => Sum(this, x)
  }

  final def -(x: Expr[T]): Sum[T] = (this, x) match {
    case (Sum(y@_*), Sum(z@_*)) => Sum(y ++ z.map(-_): _*)
    case (Sum(y@_*), _) => Sum(y :+ -x: _*)
    case (_, Sum(z@_*)) => Sum(Seq(this) ++ z.map(-_): _*)
    case (_, _) => Sum(this, -x)
  }

  final def unary_- : Expr[T] = this match {
    case Minus(x) ⇒ x
    case y ⇒ Minus(y)
  }

  final def *(x: Expr[T]): Product[T] = (this, x) match {
    case (Product(y@_*), Product(z@_*)) => Product(y ++ z: _*)
    case (Product(y@_*), _) => Product(y :+ x: _*)
    case (_, Product(z@_*)) => Product(Seq(this) ++ z: _*)
    case (_, _) => Product(this, x)
  }

  // The '#' character is needed for precedence
  final def #^(d: Int): IntPow[T] = IntPow(this, Const(d))

  def toValue: T

  final def toFunc: Seq[Double] => Double = {
    val vars = freeVars.toSeq.map(_.name.name).sorted
    ScalaCompiledFunction.compile[Seq[Double] => Double](s"case Seq(${vars.mkString(",")})" -> printToScala,
      "Seq[Double] ⇒ Double")
  }

  final def diff(x: Var[T]): Expr[T] = diffInternal(x).simplify

  private[symcal] def diffInternal(x: Var[T]): Expr[T]

  def simplify: Expr[T]

  final def subs(v: Var[T], e: Expr[T]): Expr[T] = subsInternal(v, e).simplify

  private[symcal] def subsInternal(v: Var[T], e: Expr[T]): Expr[T]

  def precedenceLevel: Int

  /** This type describes a printing function that will correctly add parentheses
    * around the expression if the expression's own precedence is lower than the outer precedence.
    */
  private[symcal] type SubExprPrinter = (Expr[T], Int) ⇒ String

  /** This type describes a printing function that, given a subexpression printer, will produce a printed
    * representation with correctly inserted parentheses at all nesting levels of the expression.
    */
  private[symcal] type PrinterWithPrecedence = (SubExprPrinter ⇒ String)

  /** Internal method that produces a printed representation of the expression.
    * It is parameterized by a [[SubExprPrinter]],
    * which will be used to print sub-expressions (that is, any parts
    * of the main expression that is itself an [[Expr]] and needs to be traversed)
    * using a specified outer precedence.
    *
    * Note that this method has type [[Expr]]`[T] ⇒ `[[PrinterWithPrecedence]], if we count the implicit `this` argument.
    *
    * Case classes extending [[Expr]] need to implement `printerForText()` and `printerForScala()`
    * by calling `subexprPrinter()` whenever they need to print any sub-expressions.
    *
    * @param subexprPrinter A function that prints subexpressions using a given outer precedence.
    * @return String representation of expression.
    */
  protected def printerForText(subexprPrinter: SubExprPrinter): String

  /** This method is the same as `printerForText` except for [[IntPow]] where an override is given. 
    * To avoid code duplication, the traversal is implemented such that the logic of printing
    * either to Scala code or to plain text is separate from the logic of adding parentheses
    * and tracking the precedence level. The methods `printerForText` and `printerForScala` encapsulate
    * all business logic that describes the respective syntax conventions, but these methods are not recursive.
    * The recursive traversal is encapsulated by `runPrinter`.
    */
  protected def printerForScala(subexprPrinter: SubExprPrinter): String = printerForText(subexprPrinter)

  final def print: String = runPrinter(_.printerForText)

  final def printToScala: String = runPrinter(_.printerForScala)

  /** Auxiliary method used to convert an [[Expr]] to a printable representation,
    * with additional flexibility of either plain text or Scala code syntax.
    *
    * @param getPrinter      A function that, given an [[Expr]], produces either a `printerForText` or a `printerForScala` function.
    * @param outerPrecedence Precedence level of the outer expression that encloses this expression.
    *                        When there is no outer expression, use the value `Expr.zeroPrecedence`, which is the default.
    * @return A string representation of the expression `this`.
    */
  final private def runPrinter(
    getPrinter: Expr[T] ⇒ PrinterWithPrecedence,
    outerPrecedence: Int = Expr.zeroPrecedence
  ): String = {
    val subexprPrinter: SubExprPrinter = { (ex, precedence) ⇒
      // Call `runPrinter` recursively on `ex`, with the specified precedence, and with the same `getPrinter`.
      // This will be called during the recursive traversal of the expression tree, on each subexpression `ex`.
      ex.runPrinter(getPrinter, precedence)
    }
    // The call `getPrinter(this)` yields a `printerForText` or a `printerForScala`,
    // whose implementations also depend on which case class `this` is.
    val printer: PrinterWithPrecedence = getPrinter(this)

    // Now we use the printer to produce the correct string representation of the inner expression,
    // so far without the outer parentheses.
    val innerFormatted: String = printer(subexprPrinter)

    // Add the outer parentheses when needed.
    if (precedenceLevel < outerPrecedence)
      "(" + innerFormatted + ")"
    else
      innerFormatted
  }

  final def freeVars: Set[Var[T]] = Expr.freeVars(this)

  def isConst: Boolean = false

  def isSum: Boolean = false

  def isProduct: Boolean = false

  final def expand: Expr[T] = expandInternal.simplify

  private[symcal] def expandInternal: Sum[T]
}

object Expr {
  /** Note:
    * If this implicit conversion is moved to `package.scala`, the expression 4 + Var('x) does not compile.
    */
  implicit def valueToConst[T: Ring : Eq](x: T): Const[T] = Const(x)

  /** These constants describe the precedence of various infix operators.
    * Parentheses are inserted whenever an operation with low precedence is enclosed
    * in a context with higher precedence.
    * Additionally, certain operations use special conventions depending on whether
    * the enclosed operation is on the left or on the right, etc.
    *
    * To describe this, we use the concept of **outer precedence**.
    * Each expression (constant, add, subtract, etc.) has a `precedenceLevel`.
    * A **subexpression** is an expression that is enclosed within another expression.
    * For example, `2 * (x + 1)` is an expression with subexpressions `2` and `x + 1`.
    *
    * Suppose we have a subexpression with precedence `p` enclosed within another expression,
    * and we need to determine whether parentheses need to be inserted around the subexpression.
    * We define the **outer precedence of the subexpression** as a certain effective precedence value `q`
    * such that parentheses need to be inserted if and only if `p < q`.
    *
    * In most cases, `q` will be equal to the precedence of the enclosing expression.
    * In some cases, such as subtraction or powers, this is not true any more, and instead `q` needs
    * to be defined in a special way.
    */
  final val zeroPrecedence = 0
  final val precedenceOfAdd = 20
  final val precedenceOfSubtract = 30
  final val precedenceOfMinus = 40
  final val precedenceOfMultiply = 50
  final val precedenceOfIntPow = 60
  final val precedenceOfConst = 100

  final def freeVars[T: Ring : Eq](e: Expr[T]): Set[Var[T]] = e match {
    case Const(_) ⇒ Set()
    case Minus(x) ⇒ freeVars(x)
    case Var(name) ⇒ Set(Var(name))
    case IntPow(x, _) ⇒ freeVars(x)
    case Sum(es@_*) ⇒ es.flatMap(e ⇒ freeVars(e)).toSet
    case Product(es@_*) ⇒ es.flatMap(e ⇒ freeVars(e)).toSet
  }

  final def constZero[T: Ring : Eq] = Const(implicitly[Ring[T]].zero)

  final def constOne[T: Ring : Eq] = Const(implicitly[Ring[T]].one)

  implicit final def intToConst[T: Ring : Eq](x: Int): Const[T] = Const(implicitly[Ring[T]].fromInt(x))

  /** Auxiliary function that returns the sequence of coefficients and powers for multinomial expansion.
    *
    * For example, to compute the expansion `(x + y)^3` we call `getTermCoeffs(2, 3)`, which returns the sequence
    * {{{
    *   Seq(
    *     (1, Seq(3, 0)),
    *     (3, Seq(2, 1)),
    *     (3, Seq(1, 2)),
    *     (1, Seq(0, 3))
    *   )
    * }}}
    *
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

  private[symcal] def subsInternal(v: Var[T], e: Expr[T]): Expr[T] = this

  override def precedenceLevel: Int = Expr.precedenceOfConst

  protected def printerForText(subexprPrinter: SubExprPrinter): String = value.toString

  override def simplify: Expr[T] = this

  override def isConst: Boolean = true

  private[symcal] def expandInternal: Sum[T] = Sum(this)
}

final case class Minus[T: Ring : Eq](x: Expr[T]) extends Expr[T] {
  override def toValue: T = -x.toValue

  private[symcal] def diffInternal(z: Var[T]): Expr[T] = -x.diff(z)

  private[symcal] def subsInternal(v: Var[T], e: Expr[T]): Expr[T] = (-x.subsInternal(v, e)).simplify

  override def precedenceLevel: Int = Expr.precedenceOfMinus

  protected def printerForText(subexprPrinter: SubExprPrinter): String =
    "-" + subexprPrinter(x, precedenceLevel)

  override def simplify: Expr[T] = x.simplify match {
    case Const(a) ⇒ Const(-a)
    case Minus(a) ⇒ a
    case xs => Minus(xs)
  }

  private[symcal] def expandInternal: Sum[T] = Sum(x.expandInternal.es.map(-_): _*)
}

final case class Var[T: Ring : Eq](name: Symbol) extends Expr[T] {
  override def toValue: T =
    throw new Exception(s"Cannot evaluate toValue for an expression containing a variable ${name.name}.")

  private[symcal] def diffInternal(x: Var[T]): Expr[T] = {
    if (name == x.name) Expr.constOne[T] else Expr.constZero[T]
  }

  private[symcal] def subsInternal(v: Var[T], e: Expr[T]): Expr[T] = v match {
    case Var(`name`) ⇒ e
    case _ ⇒ this
  }

  protected override def printerForText(subexprPrinter: SubExprPrinter): String = name.name

  override def precedenceLevel: Int = Expr.precedenceOfConst

  override def simplify: Expr[T] = this

  private[symcal] def expandInternal: Sum[T] = Sum(this)
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

  private[symcal] def subsInternal(v: Var[T], e: Expr[T]): Expr[T] = IntPow(x.subsInternal(v, e), d).simplify

  protected def printerForText(subexprPrinter: SubExprPrinter): String =
    subexprPrinter(x, precedenceLevel + 1) + "^" + d.toValue.toString

  protected override def printerForScala(subexprPrinter: SubExprPrinter): String =
    s"scala.math.pow(${subexprPrinter(x, Expr.zeroPrecedence)}, ${d.toValue.toString})"

  override def precedenceLevel: Int = Expr.precedenceOfIntPow

  private[symcal] def expandInternal: Sum[T] =
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

  private[symcal] def subsInternal(v: Var[T], e: Expr[T]): Expr[T] = Sum(es.map(_.subsInternal(v, e)): _*)

  override def precedenceLevel: Int = Expr.precedenceOfAdd

  protected def printerForText(subexprPrinter: SubExprPrinter): String = es.headOption match {
    case None ⇒ "0" // empty Sum()
    case Some(head) ⇒ subexprPrinter(head, precedenceLevel) +
      es.drop(1).map {
        case Minus(t) ⇒ " - " + subexprPrinter(t, Expr.precedenceOfMultiply)
        case t ⇒ " + " + subexprPrinter(t, precedenceLevel)
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

  def expandInternal: Sum[T] = Sum(es.flatMap(_.expandInternal.es): _*)
}

final case class Product[T: Ring : Eq](es: Expr[T]*) extends Expr[T] {
  override def isProduct: Boolean = true

  override def toValue: T = es.map(_.toValue).reduceOption(_ * _).getOrElse(implicitly[Ring[T]].one)

  private[symcal] def diffInternal(x: Var[T]): Expr[T] = {
    val diffs = es.map(_.diff(x))
    val replaced = diffs.zipWithIndex.map { case (expr, index) ⇒ Product(es.updated(index, expr): _*) }
    Sum(replaced: _*)
  }

  private[symcal] def subsInternal(v: Var[T], e: Expr[T]): Expr[T] = Product(es.map(_.subsInternal(v, e)): _*)

  override def precedenceLevel: Int = Expr.precedenceOfMultiply

  protected def printerForText(subexprPrinter: SubExprPrinter): String =
    es.map(subexprPrinter(_, precedenceLevel)).mkString(" * ")

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

  private[symcal] def expandInternal: Sum[T] = (es.headOption, es.drop(1)) match {
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
