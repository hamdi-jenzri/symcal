package com.github.symcal

object ScalaCompiledFunction {
  def compile[T](arg: (String, String), funcType: String): T = {
    // this uses the dependency "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided
    import reflect.runtime.currentMirror
    import tools.reflect.ToolBox
    val toolbox = currentMirror.mkToolBox()

    val (x, exprUsingX) = arg
    val scalaCode = s"{ $x => $exprUsingX }: ($funcType)"
    val tree = toolbox.parse(scalaCode)
    val compiledCode = toolbox.compile(tree)
    compiledCode().asInstanceOf[T]
  }
}
