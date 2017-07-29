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
    (z + y).print shouldEqual "z + 1 + 1"
  }

  behavior of "automatic simplify"

  it should "run test example from README" in {
    val x = Var('x)
    val y = Var('y)
    val z = Var('z)

    val s = (x + y) * 2 * z
    s.subs(x, 3).print shouldEqual "(3 + y) * 2 * z"

    val t = s.subs(z, 3).diff(y)
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
    ((x + 1) * ('x + 2)).subs(x, 3).toInt shouldEqual 20

    val y: Var = 'y
    val ex1 = ('x + 1) * (2 * x + y)
    val ex2 = ex1.subs(x, 3)
    val ex3 = ex2.subs(y, 4)
    ex2 shouldEqual 4 * (6 + y)
    ex3 shouldEqual Const(40)
  }

  behavior of "printed"

  it should "produce correct parentheses for simple expressions" in {
    ('x + 1).print shouldEqual "x + 1"
    ('x + 'y).print shouldEqual "x + y"
    ('x + 'y + 'z).print shouldEqual "x + y + z"
    ('x * 2).print shouldEqual "x * 2"
    (Const(2) * 'x).print shouldEqual "2 * x"
    ('x * 'y).print shouldEqual "x * y"
    ('x * 'y * 2).print shouldEqual "x * y * 2"
  }

  it should "produce correct parentheses for compound expressions" in {
    (('x + 1) * 2).print shouldEqual "(x + 1) * 2"
    ('y * ('x + 1) * 2).print shouldEqual "y * (x + 1) * 2"
    (('x + 1) * ('x + 2) * ('x + 3)).print shouldEqual "(x + 1) * (x + 2) * (x + 3)"
  }

  it should "produce correct parentheses for IntPow" in {
    ('x #^ 2).print shouldEqual "x^2"
    (Const(3) #^ 2).print shouldEqual "3^2"
    ('x #^ 2 + 1).print shouldEqual "x^2 + 1"
    ('x #^ 2 * 'x).print shouldEqual "x^2 * x"
    (('x + 1) #^ 2).print shouldEqual "(x + 1)^2"
    (('x * ('y + 1) + 2) #^ 2).print shouldEqual "(x * (y + 1) + 2)^2"
    (('x #^ 3) #^ 2).print shouldEqual "(x^3)^2"
  }

  behavior of "subtract and minus"

  it should "produce correct parentheses" in {
    (-Const(1)).print shouldEqual "-1"
    ('x - 1).print shouldEqual "x - 1"
    (-'x).print shouldEqual "-x"
    (-('x + 1)).print shouldEqual "-(x + 1)"
    (-('x - 1)).print shouldEqual "-(x - 1)"
    (-(-'x - 1)).print shouldEqual "-(-x - 1)"
    ('x * (-'y)).print shouldEqual "x * (-y)"
    ('x * ('z - 'y)).print shouldEqual "x * (z - y)"
    ('x - ('z + 'y)).print shouldEqual "x - (z + y)"
    ('x + ('z - 'y)).print shouldEqual "x + z - y"
    ('x - ('z - 'y)).print shouldEqual "x - (z - y)"
    (('x + 'z) - 'y).print shouldEqual "x + z - y"
    (-('x + 'z) - 'y).print shouldEqual "-(x + z) - y"
  }

  it should "simplify double minus" in {
    -(-'x) shouldEqual Var('x)
  }

  it should "simplify constants" in {
    (Const(1) - 2 + 3).simplify shouldEqual Const(2)
    (-Const(1) - 2 + 3).simplify shouldEqual Const(0)
    (-('x - 1)).subs(Var('x), 0).toInt shouldEqual 1
    (-('x - 1)).diff('x).toInt shouldEqual -1
    (-(-'x)).simplify shouldEqual ('x: Expr)
  }

  it should "do everything" in {
    val x = Var('x)
    val y = Var('y)
    val p = x * x - y * y + 10 * x + 20000
    p.diff('x) shouldEqual x + x + 10
    val q = p.subs(y, x + 1)
    q shouldEqual x * x - (x + 1) * (x + 1) + 10 * x + 20000
    q.subs('x, 2).toInt shouldEqual 20015
  }

  behavior of "freeVars"

  it should "compute empty set for an expression with no variables" in {
    val x = Const(1)
    Expr.freeVars(x) shouldEqual Set[Var]()
  }

  it should "compute a set of vars" in {
    var x = Var('x)
    var y = Var('y)

    val p = x * y - 1
    Expr.freeVars(p) shouldEqual Set(x, y)
    val q = x #^ 2
    q.freeVars shouldEqual Set(x)
  }

  behavior of "Sum"

  it should "print all summands" in {
    val x = Sum(1, 2, 3)
    x.print shouldEqual "1 + 2 + 3"
    val y1 = Sum(1, 'x * 'z, 'y)
    y1.print shouldEqual "1 + x * z + y"
    val y = Sum(1, 'x + 'z, 'y)
    y.print shouldEqual "1 + x + z + y"
    val s: Seq[Expr] = Seq(1, Sum('x + 'z, 'x + 3), 'y)
    val z = Sum(s: _*)
    z.print shouldEqual "1 + x + z + x + 3 + y"
    val t = 'x * z
    t.print shouldEqual "x * (1 + x + z + x + 3 + y)"
  }

  it should "print minus correctly" in {
    ('a + (-'b)).print shouldEqual "a - b"
    ('a - (-'b)).print shouldEqual "a - (-b)"
  }

  it should "compute derivative" in {
    val s: Seq[Expr] = Seq(1, Sum('x * 'z * 'x, 3 + 'x), 'y)
    val z = Sum(s: _*)
    val z_diff_x = z.diff('x)
    z_diff_x.print shouldEqual "z * x + x * z + 1"
  }

  it should "simplify constants correctly" in {
    // empty sum
    Sum().simplify shouldEqual Const(0)
    // just one zero
    Sum(0).simplify shouldEqual Const(0)
    // only constants yield a Const
    Sum(0, 1, 2, 0, 0, 0, 3).simplify shouldEqual Const(6)

    // only non-constants
    Sum('x, 'y, -'x, 'y).simplify shouldEqual Sum('x, 'y, -'x, 'y)

    // both constants and non-constants
    Sum('x, 1, 2, 'x, 0, 'x, 0, 0, 3).simplify shouldEqual Sum('x, 'x, 'x, 6)
  }

  it should "convert to int" in {
    Sum(0, 1, 2, 0, 0, 0, 3).toInt shouldEqual 6

    the[Exception] thrownBy {
      Sum(0, 1, 2, 0, 'z, 0, 'x, 3).toInt shouldEqual 6
    } should have message "Cannot evaluate toInt for an expression containing a variable z."
  }

  it should "substitute everywhere" in {
    Sum('x, 1, 'x, 2).subs('x, 'z) shouldEqual Sum('z, 1, 'z, 2)
  }

  behavior of "Product"

  it should "print all multiplicands" in {
    val x = Product(1, 2, 3)
    x.print shouldEqual "1 * 2 * 3"
    val y1 = Product(1, 'x + 'z, 'y)
    y1.print shouldEqual "1 * (x + z) * y"
    val y = Product(1, 'x * 'z, 'y)
    y.print shouldEqual "1 * x * z * y"
    val s: Seq[Expr] = Seq(1, Product('x + 'z, 'x + 3, 'z), 'y)
    val z = Product(s: _*)
    z.print shouldEqual "1 * (x + z) * (x + 3) * z * y"
    val t = 'x * z
    t.print shouldEqual "x * 1 * (x + z) * (x + 3) * z * y"
  }

  it should "compute derivative" in {
    Product(1, Product('x * 'z * 'x, 3 + 'x), 'y).diff('x).print shouldEqual "((z * x + x * z) * (3 + x) + x * z * x) * y"
  }

  it should "simplify constants correctly" in {
    // empty sum
    Product().simplify shouldEqual Const(1)
    // just one zero
    Product(0).simplify shouldEqual Const(0)
    Product(1).simplify shouldEqual Const(1)
    // only constants yield a Const
    Product(0, 1, 2, 0, 0, 0, 3).simplify shouldEqual Const(0)
    Product(1, 1, 2, 1, 1, 1, 3).simplify shouldEqual Const(6)

    // only non-constants
    Product('x, 'y, -'x, 'y).simplify shouldEqual Product('x, 'y, -'x, 'y)

    // both constants and non-constants
    Product('x, 1, 2, 'x, 0, 'x, 1, 1, 3).simplify shouldEqual Const(0)
    Product('x, 1, 2, 'x, 1, 'x, 1, 1, 3).simplify shouldEqual Product('x, 'x, 'x, 6)
  }

  it should "convert to int" in {
    Product(1, 2, 1, 1, 1, 1, 3).toInt shouldEqual 6

    the[Exception] thrownBy {
      Product(0, 1, 2, 0, 'z, 0, 'x, 3).toInt shouldEqual 6
    } should have message "Cannot evaluate toInt for an expression containing a variable z."
  }

  it should "substitute everywhere" in {
    Product('x, 1, 'x, 2).subs('x, 'z) shouldEqual Product('z, 1, 'z, 2)
  }

  behavior of "expand"

  it should "expand monomials without change" in {
    Const(1).expand shouldEqual Const(1)
    ('x * 'y).expand.print shouldEqual "x * y"
    (-'x).expand shouldEqual Minus(Var('x))
    (-'x * 'y).expand shouldEqual Minus(Var('x)) * Var('y)
  }

  it should "expand sums" in {
    ('a + 'b).expand shouldEqual Sum(Var('a), Var('b))
    ('a - ('a - 'b)).expand shouldEqual Sum(Var('a), -Var('a), Var('b))
    ('a - ('a - 'b)).expand.print shouldEqual "a - a + b"
    ('a * 'a + 'a * 'b + 'b * 'a + 'b * 'b).expand.print shouldEqual "a * a + a * b + b * a + b * b"
    (-('a + 'b)).expand shouldEqual Sum(-'a, -'b)
    (-('a - 'b)).expand shouldEqual Sum(-'a, 'b)
  }

  it should "expand products" in {
    (('a + 'b) * ('a + 'b)).expand.print shouldEqual "a * a + a * b + b * a + b * b"
    (('a + 1) * ('a + 1) * ('a + 1)).expand.print shouldEqual "a * a * a + a * a + a * a + a + a * a + a + a + 1"
  }

  behavior of "expand for power"

  it should "compute correct multinomial coefficients" in {
    IntPow('x, 2).getTermCoeffs(2) shouldEqual Seq((1, Seq(2, 0)), (2, Seq(1, 1)), (1, Seq(0, 2)))
    IntPow('x, 3).getTermCoeffs(2) shouldEqual Seq((1, Seq(3, 0)), (3, Seq(2, 1)), (3, Seq(1, 2)), (1, Seq(0, 3)))
  }

  it should "expand powers" in {
    (('x + 'y) #^ 0).expand.print shouldEqual "1"
    (('x + 'y) #^ 1).expand.print shouldEqual "x + y"
    (('x + 'y) #^ 2).expand.print shouldEqual "x^2 + x * y * 2 + y^2"
    (('x + 'y) #^ 3).expand.print shouldEqual "x^3 + x^2 * y * 3 + x * y^2 * 3 + y^3"
    (('a + 'b + 'c) #^ 3).expand.print shouldEqual "a^3 + a^2 * b * 3 + a^2 * c * 3 + a * b^2 * 3 + a * b * c * 6 + a * c^2 * 3 + b^3 + b^2 * c * 3 + b * c^2 * 3 + c^3"
    (('x + 'y + 2) #^ 5).expand.print shouldEqual "x^5 + x^4 * y * 5 + x^4 * 10 + x^3 * y^2 * 10 + x^3 * y * 40 + x^3 * 40 + x^2 * y^3 * 10 + x^2 * y^2 * 60 + x^2 * y * 120 + x^2 * 80 + x * y^4 * 5 + x * y^3 * 40 + x * y^2 * 120 + x * y * 160 + x * 80 + y^5 + y^4 * 10 + y^3 * 40 + y^2 * 80 + y * 80 + 32"
  }
}
