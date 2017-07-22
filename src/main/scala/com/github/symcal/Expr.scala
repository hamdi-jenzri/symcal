package com.github.symcal

import scala.language.implicitConversions

sealed trait Expr {
  def +(x: Expr): Expr = Add(this, x)

  def -(x: Expr): Expr = Subtract(this, x)

  def unary_- : Expr = Minus(this)

  def *(x: Expr): Expr = Multiply(this, x)

  // The '#' character is needed for precedence
  def #^(d: Int): Expr = IntPow(this, d)

  def toInt: Int

  final def diff(x: Var): Expr = diffInternal(x).simplify

  private[symcal] def diffInternal(x: Var): Expr

  def simplify: Expr

  def subs(v: Var, e: Expr): Expr

  final private[symcal] def stringForm(level: Int): String =
    if (precedenceLevel < level)
      "(" + toStringInternal + ")"
    else
      toStringInternal

  def precedenceLevel: Int

  protected def toStringInternal: String

  override final def toString: String = stringForm(0)

  def freeVars: Set[Var] = Expr.freeVars(this)

  def isConst: Boolean = false
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
}

final case class Const(value: Int) extends Expr {
  override def toInt: Int = value

  private[symcal] def diffInternal(x: Var): Expr = Const(0)

  override def subs(v: Var, e: Expr): Expr = this

  override def precedenceLevel: Int = Expr.precedenceOfConst

  override def toStringInternal: String = value.toString

  override def simplify: Expr = this

  override def isConst: Boolean = true
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

  override protected def toStringInternal: String = x.stringForm(Expr.precedenceOfAdd) + " - " + y.stringForm(Expr.precedenceOfMultiply)
}

final case class Minus(x: Expr) extends Expr {
  override def toInt: Int = -x.toInt

  private[symcal] def diffInternal(z: Var): Expr = -x.diff(z)

  override def subs(v: Var, e: Expr): Expr = (-x.subs(v, e)).simplify

  override def precedenceLevel: Int = Expr.precedenceOfMinus

  override protected def toStringInternal: String = "-" + x.stringForm(precedenceLevel)

  override def simplify: Expr = x.simplify match {
    case Const(a) ⇒ Const(-a)
    case Minus(a) ⇒ a
    case xs => Minus(xs)
  }
}

//case class FlatSum(xs: IndexedSeq[Expr])

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

  override def toStringInternal: String = x.stringForm(precedenceLevel) + " + " + y.stringForm(precedenceLevel)

  override def precedenceLevel: Int = Expr.precedenceOfAdd
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

  override def toStringInternal: String = x.stringForm(precedenceLevel) + " * " + y.stringForm(precedenceLevel)

  override def precedenceLevel: Int = Expr.precedenceOfMultiply
}

final case class Var(name: Symbol) extends Expr {
  override def toInt: Int =
    throw new Exception(s"Cannot evaluate toInt for an expression containing a variable ${name.name}.")

  private[symcal] def diffInternal(x: Var): Expr = if (name == x.name) Const(1) else Const(0)

  override def subs(v: Var, e: Expr): Expr = v match {
    case Var(`name`) ⇒ e
    case _ ⇒ this
  }

  override def toStringInternal: String = name.name

  override def precedenceLevel: Int = Expr.precedenceOfConst

  override def simplify: Expr = this
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

  override def toStringInternal: String = x.stringForm(precedenceLevel + 1) + "^" + d.toString

  override def precedenceLevel: Int = Expr.precedenceOfIntPow
}

final case class Sum(es: Expr*) extends Expr {
  override def toInt: Int = es.map(_.toInt).sum

  private[symcal] def diffInternal(x: Var): Expr = Sum(es.map(_.diff(x)): _*)

  override def subs(v: Var, e: Expr): Expr = Sum(es.map(_.subs(v, e)): _*)

  override def precedenceLevel: Int = Expr.precedenceOfAdd

  override protected def toStringInternal: String = es.map(_.stringForm(precedenceLevel)).mkString(" + ")

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
}

final case class Product(es: Expr*) extends Expr {
  override def toInt: Int = es.map(_.toInt).product

  override def diffInternal(x: Var): Expr = ???

  override def subs(v: Var, e: Expr): Expr = Product(es.map(_.subs(v, e)): _*)

  override def precedenceLevel: Int = Expr.precedenceOfMultiply

  override protected def toStringInternal: String = es.map(_.stringForm(precedenceLevel)).mkString(" * ")

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
          // In this case, the simplified result is not a `Sum`.
          e
        } else Product(mergedExprs: _*)
      case None ⇒ Const(1) // Empty product is transformed into `Const(1)`.
    }
  }
}
