package com.github.symcal

import org.scalatest.{FlatSpec, Matchers}
import spire.implicits.DoubleAlgebra

class ToFuncSpec extends FlatSpec with Matchers {

  it should "convert simple Expr to function" in {
    val expr: Expr[Double] = 'x + 1.0
    val f = expr.toFunc
    f(Seq(2)) shouldEqual 3
  }

  it should "convert an Expr with 2 Vars to function" in {
    val expr = 'y - 'x + 1.0
    val f = expr.toFunc
    f(Seq(2, 1)) shouldEqual 0
  }

  it should "toFunc Expr with IntPow" in {
    val x_3 = 'x #^ 3
    x_3.toFunc(Seq(2)) shouldEqual 8
  }
}
