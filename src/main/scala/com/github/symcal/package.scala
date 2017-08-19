package com.github

import spire.algebra.{Eq, Ring}

import scala.language.implicitConversions

package object symcal {

  implicit class ConstOps[T: Ring : Eq](x: T) {
    def +(z: Expr[T]): Expr[T] = Const(x) + z
    def +(z: Symbol): Expr[T] = Const(x) + z
    def *(z: Expr[T]): Expr[T] = Const(x) * z
    def *(z: Symbol): Expr[T] = Const(x) * z
    def -(z: Expr[T]): Expr[T] = Const(x) - z
    def -(z: Symbol): Expr[T] = Const(x) - z
  }

  implicit def symbolToVar[T: Ring : Eq](x: Symbol): Var[T] = Var(x)
}
