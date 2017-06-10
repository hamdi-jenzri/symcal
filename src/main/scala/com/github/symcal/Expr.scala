package com.github.symcal

trait Expr {
  def +(x: Expr): Expr = {
    Plus(this, x)
  }

  def toInt: Int

  def diff(x: Var): Expr

  override def toString: String
}

case class Const(x: Int) extends Expr {
  override def toInt: Int = x

  def diff(x: Var): Expr = Const(0)

  override def toString: String = x.toString
}

case class Plus(x: Expr, y: Expr) extends Expr {
  override def toInt: Int = x.toInt + y.toInt

  def diff(z: Var): Expr = x.diff(z) + y.diff(z)

  override def toString: String = x.toString + " + " + y.toString
}

case class Var(name: Symbol) extends Expr {
  override def toInt: Int =
    throw new Exception(s"Cannot evaluate toInt for an expression containing a variable ${name.name}.")

  def diff(x: Var): Expr = if (name == x.name) Const(1) else Const(0)

  override def toString: String = name.name
}