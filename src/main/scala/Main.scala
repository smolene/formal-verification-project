import scala.io.Source
import parser._
import logic._
import typecheck._

@main
def main(argv: String*): Unit =
  val filename = if argv.isDefinedAt(0) 
    then argv(0) 
    else "src/test/test-dl.formal"

  val f = Source.fromFile(filename)
    .getLines()
    .mkString("\n")

  val items = parse(f)
  println(items.map(_.show))

  val (itemsU, fglob, tglob) = uniqueProgram(items.get)
  println(itemsU.show)

  val tdl = toDatalog(itemsU)
  println(tdl.show)

  val outputFile = "src/test/output.formal"
  writeFile(outputFile, tdl.withHeader)

  import scala.sys.process._
  // run racket datalog engine
  val res = s"racket $outputFile" .!!
  println(s"\n-- racket output:\n$res-- end of racket output")

def writeFile(filename: String, s: String): Unit =
  import java.io._
  val file = new File(filename)
  val bw = new BufferedWriter(new FileWriter(file))
  try {
    bw.write(s)
  } finally {
    bw.close()
  }
