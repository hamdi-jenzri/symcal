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

  def diff(x: Var): Expr

  def simplify: Expr = this

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
  }
}

case class Const(value: Int) extends Expr {
  override def toInt: Int = value

  def diff(x: Var): Expr = Const(0)

  override def subs(v: Var, e: Expr): Expr = this

  override def precedenceLevel: Int = Expr.precedenceOfConst

  override def toStringInternal: String = value.toString
}

case class Subtract(x: Expr, y: Expr) extends Expr {
  override def toInt: Int = x.toInt - y.toInt

  override def diff(z: Var): Expr = (x.diff(z) - y.diff(z)).simplify

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

case class Minus(x: Expr) extends Expr {
  override def toInt: Int = -x.toInt

  override def diff(z: Var): Expr = (-x.diff(z)).simplify

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

case class Add(x: Expr, y: Expr) extends Expr {
  override def toInt: Int = x.toInt + y.toInt

  def diff(z: Var): Expr = (x.diff(z) + y.diff(z)).simplify

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

case class Multiply(x: Expr, y: Expr) extends Expr {
  override def toInt: Int = x.toInt * y.toInt

  override def diff(z: Var): Expr = (Multiply(x.diff(z), y) + Multiply(x, y.diff(z))).simplify

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

case class Var(name: Symbol) extends Expr {
  override def toInt: Int =
    throw new Exception(s"Cannot evaluate toInt for an expression containing a variable ${name.name}.")

  def diff(x: Var): Expr = if (name == x.name) Const(1) else Const(0)

  override def subs(v: Var, e: Expr): Expr = v match {
    case Var(`name`) ⇒ e
    case _ ⇒ this
  }

  override def toStringInternal: String = name.name

  override def precedenceLevel: Int = Expr.precedenceOfConst
}

case class IntPow(x: Expr, d: Const) extends Expr {
  override def toInt: Int = Math.pow(x.toInt, d.value).toInt

  override def diff(z: Var): Expr = (d match {
    case Const(0) => Const(0)
    case Const(1) => x.diff(z)
    case _ => d * x.diff(z) * IntPow(x, Const(d.value - 1))
  }).simplify

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
