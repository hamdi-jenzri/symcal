package com.github.symcal

import java.util.concurrent.atomic.AtomicInteger
import java.util.function.DoubleUnaryOperator

import net.openhft.compiler.CompilerUtils

case class CompiledFunction(f: DoubleUnaryOperator) extends AnyVal {
  def apply(x: Double): Double = f.applyAsDouble(x)
}

object CompiledFunction {
  val uniqueCounter = new AtomicInteger(0)

  def compile(arg: (String, String)): CompiledFunction = {
    val (x, exprUsingX) = arg
    val n = uniqueCounter.incrementAndGet()
    val className = s"NewCompiledFunction$n"
    val packageName = "com.github.symcal.compiled_function"
    val fqClassName = s"$packageName.$className"

    val javaCode =
      s"""
         |package $packageName;
         |import java.util.function.DoubleUnaryOperator;
         |public class $className implements DoubleUnaryOperator {
         |  public double applyAsDouble(double $x) {
         |    return ($exprUsingX);
         |  }
         |}
      """.stripMargin

    val aClass = CompilerUtils.CACHED_COMPILER.loadFromJava(fqClassName, javaCode)
    val runner = aClass.newInstance.asInstanceOf[DoubleUnaryOperator]
    CompiledFunction(runner)
  }
}
