package com.github.symcal

import org.scalatest.{FlatSpec, Matchers}

class CompiledFunctionSpec extends FlatSpec with Matchers {
  behavior of "compiled function API"

  it should "make a simple numerical function" in {
    val func = CompiledFunction.compile("x" -> "x / 2")
    func(125) shouldEqual 62.5
    func(125.0) shouldEqual 62.5
    func(0) shouldEqual 0
    func(1) shouldEqual 0.5
  }

  it should "make a function with powers" in {
    val func = CompiledFunction.compile("x" -> "Math.pow(x, 0.5)")

    func(25) shouldEqual 5
  }

  it should "compile and run a trigonometric function" in {
    val func = CompiledFunction.compile("x" -> "Math.sin(x * 3.141592653589793)")

    math.abs(func(1)) should be < 1.0e-15
    math.abs(func(5)) should be < 1.0e-15
    func(1.5) shouldEqual -1.0 // sin(3*Pi/2)
  }
}
