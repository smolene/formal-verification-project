package error

abstract class CompileException(s: String) extends RuntimeException(s)
case class CompileError(s: String) extends CompileException(s)
