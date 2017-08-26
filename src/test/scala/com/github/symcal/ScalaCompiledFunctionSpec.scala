package com.github.symcal

import org.scalatest.{FlatSpec, Matchers}

class ScalaCompiledFunctionSpec extends FlatSpec with Matchers {

  behavior of "scala compiled function"

  it should "compile and run simple functions" in {
    val func = ScalaCompiledFunction.compile[Double ⇒ Double]("x: Double" -> "x / 2", "Double ⇒ Double")
    func(125) shouldEqual 62.5
    func(125.0) shouldEqual 62.5
    func(0) shouldEqual 0
    func(1) shouldEqual 0.5
  }

  it should "measure time for compiling and running a function" in {
    val total = 100

    val compileAndRunTimes = (1 to total).map { i ⇒
      val initTime = System.nanoTime()
      val f = ScalaCompiledFunction.compile[Double ⇒ Double]("x: Double" → s"x + $i", "Double ⇒ Double")
      val compiled = System.nanoTime()
      f(125)
      val evaluated = System.nanoTime()
      (compiled - initTime, evaluated - compiled)
    }

    val averageCompileTime = compileAndRunTimes.map(_._1).sum / total
    val averageRunTime = compileAndRunTimes.map(_._2).sum / total
    println(s"Average compile time for Scala-compiled function: ${averageCompileTime / 1000000} ms; running time $averageRunTime ns")
    println(s"Best times for Scala-compiled function: compilation ${compileAndRunTimes.map(_._1).min / 1000000} ms; run time ${compileAndRunTimes.map(_._2).min} ns")
    println(s"Worst times for Scala-compiled function: compilation ${compileAndRunTimes.map(_._1).max / 1000000} ms; run time ${compileAndRunTimes.map(_._2).max} ns")
  }

  it should "measure JVM-amortized time for running a trigonometric function" in {
    val total = 100000
    val f = ScalaCompiledFunction.compile[Double ⇒ Double]("x: Double" → "scala.math.sin(x + 1)", "Double ⇒ Double")
    val resultsRaw = (1 to total).map { i ⇒
      val initTime = System.nanoTime()
      val x = f(125)
      val evaluated = System.nanoTime()
      evaluated - initTime
    }
    val results = resultsRaw.takeRight(100)
    val averageRunTime = results.sum / results.length
    println(s"Average amortized running time for Scala-compiled function: $averageRunTime ns; best time: ${resultsRaw.min} ns; worst time: ${resultsRaw.max} ns")
  }

}
