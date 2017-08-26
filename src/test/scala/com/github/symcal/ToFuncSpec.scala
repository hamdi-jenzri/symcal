package com.github.symcal

import org.scalatest.{FlatSpec, Matchers}
import spire.implicits.DoubleAlgebra
import Expr._

class ToFuncSpec extends FlatSpec with Matchers {

  behavior of "toFunc"

  it should "convert simple Expr to function" in {
    val expr: Expr[Double] = 'x + 1
    val f = expr.toFunc
    f(Seq(2)) shouldEqual 3
  }

  it should "convert an Expr with 2 Vars to function" in {
    val expr = 'y - 'x + 1
    val f = expr.toFunc
    f(Seq(2, 1)) shouldEqual 0
  }

  it should "convert Expr with IntPow" in {
    val x_3 = 'x #^ 3
    x_3.toFunc(Seq(2)) shouldEqual 8
  }

  it should "measure JVM-amortized time for running a Math.pow function" in {
    val total = 100000
    val expr = 'x #^ 3
    val f = expr.toFunc

    val resultsRaw = (1 to total).map { i ⇒
      val initTime = System.nanoTime()
      val x = f(Seq(125))
      val evaluated = System.nanoTime()
      evaluated - initTime
    }
    val results = resultsRaw.takeRight(100)
    val averageRunTime = results.sum / results.length
    println(s"Average amortized running time for $expr: $averageRunTime ns; best time: ${resultsRaw.min} ns; worst time: ${resultsRaw.max} ns")
    // on a MacBook Pro: ave. 160ns / min. 140ns with either scala.math.pow or Math.pow
  }
}
