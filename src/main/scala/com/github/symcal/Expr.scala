package com.github.symcal

import scala.language.implicitConversions

trait Expr {
  def +(x: Expr): Expr = {
    Plus(this, x)
  }

  def *(x: Expr): Expr = {
    Product(this, x)
  }

  // The '#' character is needed for precedence
  def #^(d: Int): Expr = IntPow(this, d)

  def toInt: Int

  def diff(x: Var): Expr

  def simplify: Expr = this

  def subs(subExpr: (Var, Expr)): Expr

  override def toString: String
}

object Expr {
  implicit def intToConst(x: Int): Const = {
    Const(x)
  }
}

case class Const(value: Int) extends Expr {
  override def toInt: Int = value

  def diff(x: Var): Expr = Const(0)

  override def subs(subExpr: (Var, Expr)): Expr = this

  override def toString: String = value.toString
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

  override def toString: String = x.toString + " + " + y.toString
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

  override def toString: String = s"($x) * ($y)"
}

case class Var(name: Symbol) extends Expr {
  override def toInt: Int =
    throw new Exception(s"Cannot evaluate toInt for an expression containing a variable ${name.name}.")

  def diff(x: Var): Expr = if (name == x.name) Const(1) else Const(0)

  override def subs(subExpr: (Var, Expr)): Expr = subExpr match {
    case (Var(`name`), expr) ⇒ expr
    case _ ⇒ this
  }

  override def toString: String = name.name
}

case class IntPow(x: Expr, d: Int) extends Expr {
  override def toInt: Int = Math.pow(x.toInt, d).toInt

  override def diff(z: Var): Expr = (d match {
    case 0 => Const(0)
    case 1 => x.diff(z)
    case _ => d * x.diff(z) * IntPow(x, d - 1)
  }).simplify

  override def simplify: Expr = (x.simplify, d) match {
    case (Const(a), _) => Const(IntPow(Const(a), d).toInt)
    case (xs, 1) => xs
    case (_, 0) => Const(1)
    case (xs, _) ⇒ IntPow(xs, d)
  }

  override def subs(subExpr: (Var, Expr)): Expr = IntPow(x.subs(subExpr), d).simplify

  override def toString: String = s"($x)^$d"
}