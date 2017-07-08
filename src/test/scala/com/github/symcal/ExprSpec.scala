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
    (x + y + x + z).diff(x).simplify shouldEqual Const(2)
  }

  it should "compute diff of a product" in {
    val x = Var('x)
    val y = Var('y)
    val z = Const(1)
    val u = Const(3)
    (x * (y + z)).diff(x).simplify shouldEqual y + z
    (x * (y + z)).diff(y).simplify shouldEqual x
    (x * y * x).diff(x).simplify shouldEqual y * x + x * y
    (z * u).toInt shouldEqual 3
    (z * u).simplify shouldEqual u
  }

  it should "compute diff of power" in {
    val x = Var('x)
    val y = Var('y)
    val z = Const(1)
    (x#^4).diff(x).simplify shouldEqual 4 * x#^3
    (Const(3)#^2).toInt shouldEqual 9
  }
}
