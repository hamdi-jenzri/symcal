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

  behavior of "automatic simplify"

  it should "run test example from README" in {
    val x = Var('x)
    val y = Var('y)
    val z = Var('z)

    val s = (x + y)*2*z
    s.subs(x -> 3).toString shouldEqual "(3 + y) * 2 * z"

    val t = s.subs(z -> 3).diff(y)
    t.toInt shouldEqual 6
  }

  it should "simplify constants" in {
    (Const(0) + Const(1) + Const(0) + Const(2)).simplify shouldEqual Const(3)
    ((Const(0) * Const(3) + Const(2) * Const(2)) * Const(2)).simplify shouldEqual Const(8)
    (0 + 'x + 1).simplify shouldEqual 'x + 1
    (1 + 0 + 'x + 1).simplify shouldEqual 1 + 'x + 1
  }

  behavior of "derivative"

  it should "for Plus" in {
    val x = Var('x)
    val y = Var('y)
    val z = Const(1)
    (x + y + x + z).diff(x) shouldEqual Const(2)
  }

  it should "for Product" in {
    val x = Var('x)
    val y = Var('y)
    val z = Const(1)
    val u = Const(3)
    (x * (y + z)).diff(x) shouldEqual y + z
    (x * (y + z)).diff(y) shouldEqual x
    (x * y * x).diff(x) shouldEqual y * x + x * y
    (z * u).toInt shouldEqual 3
    (z * u).simplify shouldEqual u
  }

  it should "for IntPow" in {
    val x = Var('x)
    val y = Var('y)
    val z = Const(1)
    (x #^ 4).diff(x) shouldEqual 4 * x #^ 3
    (Const(3) #^ 2).simplify shouldEqual Const(9)
    (Const(3) #^ 2).toInt shouldEqual 9
    (z #^ 3).simplify shouldEqual Const(1)
    (Const(3) #^ 1).simplify shouldEqual Const(3)
    (Const(3) #^ 0).simplify shouldEqual Const(1)
    (Const(0) #^ 0).simplify shouldEqual Const(1)
    (Const(0) #^ 3).simplify shouldEqual Const(0)
  }

  behavior of "substitution"

  it should "evaluate simple expressions" in {
    val x: Var = 'x
    // Here, x and 'x are the same variable and should be both substituted at once.
    ((x + 1) * ('x + 2)).subs(x → 3).toInt shouldEqual 20

    val y: Var = 'y
    val ex1 = ('x + 1) * (2 * x + y)
    val ex2 = ex1.subs(x → 3)
    val ex3 = ex2.subs(y → 4)
    ex2 shouldEqual 4 * (6 + y)
    ex3 shouldEqual Const(40)
  }

  behavior of "toString"

  it should "produce correct parentheses for simple expressions" in {
    ('x + 1).toString shouldEqual "x + 1"
    ('x + 'y).toString shouldEqual "x + y"
    ('x + 'y + 'z).toString shouldEqual "x + y + z"
    ('x * 2).toString shouldEqual "x * 2"
    (Const(2) * 'x).toString shouldEqual "2 * x"
    ('x * 'y).toString shouldEqual "x * y"
    ('x * 'y * 2).toString shouldEqual "x * y * 2"
  }

  it should "produce correct parentheses for compound expressions" in {
    (('x + 1) * 2).toString shouldEqual "(x + 1) * 2"
    ('y * ('x + 1) * 2).toString shouldEqual "y * (x + 1) * 2"
    (('x + 1) * ('x + 2) * ('x + 3)).toString shouldEqual "(x + 1) * (x + 2) * (x + 3)"
  }

  it should "produce correct parentheses for IntPow" in {
    ('x #^ 2).toString shouldEqual "x^2"
    (Const(3) #^ 2).toString shouldEqual "3^2"
    ('x #^ 2 + 1).toString shouldEqual "x^2 + 1"
    ('x #^ 2 * 'x).toString shouldEqual "x^2 * x"
    (('x + 1) #^ 2).toString shouldEqual "(x + 1)^2"
    (('x * ('y + 1) + 2) #^ 2).toString shouldEqual "(x * (y + 1) + 2)^2"
    (('x #^ 3) #^ 2).toString shouldEqual "(x^3)^2"
  }
}
