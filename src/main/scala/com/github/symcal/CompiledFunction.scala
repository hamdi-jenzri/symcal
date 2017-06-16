package com.github.symcal

import java.util.concurrent.atomic.AtomicInteger
import java.util.function.Function

import net.openhft.compiler.CompilerUtils

trait CompiledFunction extends Function1[Double, Double] {

}

object CompiledFunction {

  val uniqueCounter = new AtomicInteger(0)

  def compile(arg: (String, String)): Function[Double, Double] = {
    val (x, exprUsingX) = arg
    val n = uniqueCounter.incrementAndGet()
    val className = s"NewCompiledFunction$n"
    val packageName = "com.github.symcal.compiled_function"
    val fqClassName = s"$packageName.$className"

    val javaCode =
      s"""
         |package $packageName;
         |import java.util.function.Function;
         |public class $className implements Function<Double, Double> {
         |  public Double apply(Double $x) {
         |    return $exprUsingX;
         |  }
         |}
      """.stripMargin
    val aClass = CompilerUtils.CACHED_COMPILER.loadFromJava(fqClassName, javaCode)
    val runner = aClass.newInstance.asInstanceOf[java.util.function.Function[Double, Double]]
    runner
  }
}
