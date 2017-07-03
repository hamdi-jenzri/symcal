package com.github.symcal

import org.scalatest.{FlatSpec, Matchers}

class JavaCompiledFunctionSpec extends FlatSpec with Matchers {
  behavior of "compiled function API"

  it should "make a simple numerical function" in {
    val func = JavaCompiledFunction.compile("x" -> "x / 2")
    func(125) shouldEqual 62.5
    func(125.0) shouldEqual 62.5
    func(0) shouldEqual 0
    func(1) shouldEqual 0.5
  }

  it should "measure time for compiling and running a function" in {
    val total = 100

    val compileAndRunTimes = (1 to total).map { i ⇒
      val initTime = System.nanoTime()
      val f = JavaCompiledFunction.compile("x" → s"x + $i")
      val compiled = System.nanoTime()
      f(125)
      val evaluated = System.nanoTime()
      (compiled - initTime, evaluated - compiled)
    }

    val averageCompileTime = compileAndRunTimes.map(_._1).sum / total
    val averageRunTime = compileAndRunTimes.map(_._2).sum / total
    println(s"Average compile time for Java-compiled function: ${averageCompileTime / 1000000} ms; running time $averageRunTime ns")
    println(s"Best times for Java-compiled function: compilation ${compileAndRunTimes.map(_._1).min / 1000000} ms; run time ${compileAndRunTimes.map(_._2).min} ns")
    println(s"Worst times for Java-compiled function: compilation ${compileAndRunTimes.map(_._1).max / 1000000} ms; run time ${compileAndRunTimes.map(_._2).max} ns")
  }

  it should "measure JVM-amortized time for running a trigonometric function" in {
    val total = 100000
    val f = JavaCompiledFunction.compile("x" → "Math.sin(x + 1)")

    val resultsRaw = (1 to total).map { i ⇒
      val initTime = System.nanoTime()
      val x = f(125)
      val evaluated = System.nanoTime()
      evaluated - initTime
    }
    val results = resultsRaw.takeRight(100)
    val averageRunTime = results.sum / results.length
    println(s"Average amortized running time for Java-compiled function: $averageRunTime ns; best time: ${resultsRaw.min} ns; worst time: ${resultsRaw.max} ns")
  }

  it should "make a function with powers" in {
    val func = JavaCompiledFunction.compile("x" -> "Math.pow(x, 0.5)")

    func(25) shouldEqual 5
  }

  it should "compile and run a trigonometric function" in {
    val func = JavaCompiledFunction.compile("x" -> "Math.sin(x * 3.141592653589793)")

    math.abs(func(1)) should be < 1.0e-15
    math.abs(func(5)) should be < 1.0e-15
    func(1.5) shouldEqual -1.0 // sin(3*Pi/2)
  }
}
