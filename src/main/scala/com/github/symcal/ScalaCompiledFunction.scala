package com.github.symcal

object ScalaCompiledFunction {
  def compile(arg: (String, String)): (Double ⇒ Double) = {
// this uses the dependency "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided
    import reflect.runtime.currentMirror
    import tools.reflect.ToolBox
    val toolbox = currentMirror.mkToolBox()

    val (x, exprUsingX) = arg

    val scalaCode =
      s"""
         |{(x: Double) =>
         |    $exprUsingX
         |}
      """.stripMargin

    val tree = toolbox.parse(scalaCode)
    val compiledCode = toolbox.compile(tree)

    compiledCode().asInstanceOf[Function1[Double, Double]]
  }
}