package com.github.symcal

import org.scalatest.{FlatSpec, Matchers}

class ScalaCompiledFunctionSpec extends FlatSpec with Matchers {

  behavior of "scala compiled function"

  it should "compile and run simple functions" in {
    val func = ScalaCompiledFunction.compile("x" -> "x / 2")
    func(125) shouldEqual 62.5
    func(125.0) shouldEqual 62.5
    func(0) shouldEqual 0
    func(1) shouldEqual 0.5
  }

  it should "measure time for compiling and running a function" in {
    val total = 100

    val compileTimes = (1 to total).map { i ⇒
      val initTime = System.nanoTime()
      val f = ScalaCompiledFunction.compile("x" → s"x + $i")
      val compiled = System.nanoTime()
      f(125)
      val evaluated = System.nanoTime()
      (compiled - initTime, evaluated - compiled)
    }

    val averageCompileTime = compileTimes.map(_._1).sum / total
    val averageRunTime = compileTimes.map(_._2).sum / total
    println(s"Average compile time for Scala-compiled function: ${averageCompileTime / 1000000} ms; running time $averageRunTime ns")
    println(s"Worst times for Scala-compiled function: compilation ${compileTimes.map(_._1).max / 1000000} ms; run time ${compileTimes.map(_._2).max} ns")
  }

  it should "measure JVM-amortized time for running a trigonometric function" in {
    val total = 10000
    val f = ScalaCompiledFunction.compile("x" → s"scala.math.sin(x + 1)")

    val resultsRaw = (1 to total).map { i ⇒
      val initTime = System.nanoTime()
      val x = f(125)
      val evaluated = System.nanoTime()
      evaluated - initTime
    }
    val results = resultsRaw.takeRight(100)
    val averageRunTime = results.sum / results.length
    println(s"Average amortized running time for Scala-compiled function: $averageRunTime ns; best time: ${results.min} ns; worst time: ${resultsRaw.max} ns")
  }

}
