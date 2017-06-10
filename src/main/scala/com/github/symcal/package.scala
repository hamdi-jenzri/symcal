package com.github


package object symcal {
  implicit class IntOps(x: Int) {
    def +(z: Expr): Expr = Const(x) + z
    def +(z: Symbol): Expr = Const(x) + z
  }

  implicit def symbolToVar(x: Symbol): Var = {
    Var(x)
  }
}
