package com.github.symcal

import org.scalatest.{FlatSpec, Matchers}
import spire.implicits.DoubleAlgebra
import Expr._

class ToFuncSpec extends FlatSpec with Matchers {

  def printResults(subject: String, results: Seq[Long]): Unit = {
    val amortizedResults = results.takeRight(results.length * 4 / 5)
    val best = results.min
    val worst = results.max
    val average = amortizedResults.sum / amortizedResults.length
    println(s"Running time for $subject: average $average ns; best: $best ns; worst: $worst ns")
  }

  def performTimings(doWork: ⇒ Unit, total: Int): Seq[Long] = {
    (1 to total).map { _ ⇒
      val initTime = System.nanoTime()
      doWork
      val evaluated = System.nanoTime()
      evaluated - initTime
    }
  }

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

  it should "convert Expr with IntPow after expansion to valid Scala code" in {
    Sum('x).print shouldEqual "x"
    Sum(1).printToScala shouldEqual "1.0"
    (Sum('x #^ 2)).printToScala shouldEqual "scala.math.pow(x, 2)"

    val x_3 = (('x + 1) #^ 2).expand
    x_3.printToScala shouldEqual "scala.math.pow(x, 2) + 2.0 * x + 1.0"
  }

  it should "measure JVM-amortized time for running a Math.pow function" in {
    val total = 100000
    val expr = 'x #^ 3
    val f = expr.toFunc

    val results = performTimings(f(Seq(125)), total)
    printResults(expr.print, results)
    // on a MacBook Pro: ave. 160ns / min. 140ns with either scala.math.pow or Math.pow
  }

  val longExpr = (('x - 1) #^ 25).expand

  it should "measure time for evaluating long expression with .subs()" in {
    longExpr.subs('x, 1).toValue shouldEqual 0.0
    printResults("long expression with .subs()", performTimings(longExpr.subs('x, 1), 100000))
  }

  it should "measure time for evaluating long expression with .toFunc()" in {
    val f = longExpr.toFunc
    val args = Seq(1.0)
    f(args) shouldEqual 0.0
    printResults("long expression with .toFunc()", performTimings(f(args), 100000))
  }

}
