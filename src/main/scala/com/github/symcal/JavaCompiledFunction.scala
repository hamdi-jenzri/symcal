package com.github.symcal

import java.util.concurrent.atomic.AtomicInteger
import java.util.function.DoubleUnaryOperator

import com.github.symcal.javacompiler.JavaInMemoryCompiler

/** Adapt [[DoubleUnaryOperator]] to the usual function interface by adding `apply()`.
  *
  * @param f A Java function.
  */
case class JavaCompiledFunction(f: DoubleUnaryOperator) extends AnyVal {
  def apply(x: Double): Double = f.applyAsDouble(x)
}

object JavaCompiledFunction {
  val uniqueCounter = new AtomicInteger(0)

  def compile(arg: (String, String)): JavaCompiledFunction = {
    val (x, exprUsingX) = arg
    val n = uniqueCounter.incrementAndGet()
    val className = s"NewCompiledFunction$n"
    val packageName = "com.github.symcal.java_compiled_function"
    val fqClassName = s"$packageName.$className"

    val javaCode: String =
      s"""
         |package $packageName;
         |import java.util.function.DoubleUnaryOperator;
         |public class $className implements DoubleUnaryOperator {
         |  public double applyAsDouble(double $x) {
         |    return ($exprUsingX);
         |  }
         |}
      """.stripMargin
    JavaInMemoryCompiler.compile(fqClassName, javaCode)
  }
}
