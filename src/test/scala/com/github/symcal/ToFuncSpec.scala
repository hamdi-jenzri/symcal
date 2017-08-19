package com.github.symcal

import org.scalatest.{FlatSpec, Matchers}
import spire.implicits.DoubleAlgebra

class ToFuncSpec extends FlatSpec with Matchers {

  it should "convert simple Expr to function" in {
    val expr = 'x + 1
    val f = expr.toFunc
    f(2) shouldEqual 3
  }

  it should "an Expr with 2 Vars" in {
    val expr = 'y - 'x + 1
    val f = expr.toFunc
    f(x = 2, y = 1) shouldEqual 0
  }
}
