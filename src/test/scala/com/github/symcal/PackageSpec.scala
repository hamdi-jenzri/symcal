package com.github.symcal

import org.scalatest.{FlatSpec, Matchers}

import spire.implicits.IntAlgebra

class PackageSpec extends FlatSpec with Matchers {

  behavior of "Implicits"

  it should "convert Integers and Symbols to Expr" in {
    val x = 'x + 1
    x.isInstanceOf[Expr[Int]] shouldEqual true
    val y: Expr[Int] = 1 + 1
    y.print shouldEqual "2"
    val z = 1 + x
    z.isInstanceOf[Expr[Int]] shouldEqual true
    z.print shouldEqual "1 + x + 1"
    val t = 1 + 'x
    t.print shouldEqual "1 + x"
  }

}
