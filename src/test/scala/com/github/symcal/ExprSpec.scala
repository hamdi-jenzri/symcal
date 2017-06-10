package com.github.symcal

import org.scalatest.{FlatSpec, Matchers}

class ExprSpec extends FlatSpec with Matchers {

  behavior of "Expr Plus"

  it should "create Expr" in {
    val x = Var('x)
    x.isInstanceOf[Expr] shouldEqual true
    val y = x + Const(1)
    y.isInstanceOf[Expr] shouldEqual true
  }

  it should "evaluate to Int" in {
    val x = Const(1)
    x.toInt shouldEqual 1
    val y = x + Const(1)
    y.toInt shouldEqual 2
    the[Exception] thrownBy {
      val z = Var('z)
      (z + y).toInt
    } should have message "Cannot evaluate toInt for an expression containing a variable z."
  }

  it should "print expressions" in {
    val x = Const(1)
    val y = x + Const(1)
    val z = Var('z)
    (z + y).toString shouldEqual "z + 1 + 1"
  }

  it should "compute derivative" in {
    val x = Var('x)
    val y = Var('y)
    val z = Const(1)
    (x + y + x + z).diff(x) shouldEqual Const(1) + Const(0) + Const(1) + Const(0)
  }
}
