package com.github.symcal

import scala.language.implicitConversions

trait Expr {
  def +(x: Expr): Expr = Plus(this, x)

  def *(x: Expr): Expr = Product(this, x)

  // The '#' character is needed for precedence
  def #^(d: Int): Expr = IntPow(this, d)

  def toInt: Int

  def diff(x: Var): Expr

  def simplify: Expr = this

  def subs(subExpr: (Var, Expr)): Expr

  final private[symcal] def stringForm(level: Int): String =
    if (precedenceLevel < level)
      "(" + toStringInternal + ")"
    else
      toStringInternal

  def precedenceLevel: Int

  protected def toStringInternal: String

  override final def toString: String = stringForm(0)
}

object Expr {
  implicit def intToConst(x: Int): Const = {
    Const(x)
  }

  final val precedenceOfConst = 100
  final val precedenceOfPlus = 20
  final val precedenceOfProduct = 40
  final val precedenceOfIntPow = 50
}

case class Const(value: Int) extends Expr {
  override def toInt: Int = value

  def diff(x: Var): Expr = Const(0)

  override def subs(subExpr: (Var, Expr)): Expr = this

  override def precedenceLevel: Int = Expr.precedenceOfConst

  override def toStringInternal: String = value.toString
}

case class Plus(x: Expr, y: Expr) extends Expr {
  override def toInt: Int = x.toInt + y.toInt

  def diff(z: Var): Expr = (x.diff(z) + y.diff(z)).simplify

  override def simplify: Expr = (x.simplify, y.simplify) match {
    case (Const(0), ys) => ys
    case (xs, Const(0)) => xs
    case (Const(a), Const(b)) => Const(a + b)
    case (xs, ys) => xs + ys
  }

  override def subs(subExpr: (Var, Expr)): Expr = (x.subs(subExpr) + y.subs(subExpr)).simplify

  override def toStringInternal: String = x.stringForm(precedenceLevel) + " + " + y.stringForm(precedenceLevel)

  override def precedenceLevel: Int = Expr.precedenceOfPlus
}

case class Product(x: Expr, y: Expr) extends Expr {
  override def toInt: Int = x.toInt * y.toInt

  override def diff(z: Var): Expr = (Product(x.diff(z), y) + Product(x, y.diff(z))).simplify

  override def simplify: Expr = (x.simplify, y.simplify) match {
    case (Const(1), ys) => ys
    case (xs, Const(1)) => xs
    case (Const(0), _) => Const(0)
    case (_, Const(0)) => Const(0)
    case (Const(a), Const(b)) => Const(a * b)
    case (xs, ys) => xs * ys
  }

  override def subs(subExpr: (Var, Expr)): Expr = (x.subs(subExpr) * y.subs(subExpr)).simplify

  override def toStringInternal: String = x.stringForm(precedenceLevel) + " * " + y.stringForm(precedenceLevel)

  override def precedenceLevel: Int = Expr.precedenceOfProduct
}

case class Var(name: Symbol) extends Expr {
  override def toInt: Int =
    throw new Exception(s"Cannot evaluate toInt for an expression containing a variable ${name.name}.")

  def diff(x: Var): Expr = if (name == x.name) Const(1) else Const(0)

  override def subs(subExpr: (Var, Expr)): Expr = subExpr match {
    case (Var(`name`), expr) ⇒ expr
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

  override def subs(subExpr: (Var, Expr)): Expr = IntPow(x.subs(subExpr), d).simplify

  override def toStringInternal: String = x.stringForm(precedenceLevel + 1) + "^" + d.toString

  override def precedenceLevel: Int = Expr.precedenceOfIntPow
}
