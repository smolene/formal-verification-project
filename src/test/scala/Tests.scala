import parser._

import scala.io.Source

class Tests extends munit.FunSuite {
  val f = Source.fromFile("src/test/test1.formal")
    .getLines()
    .mkString("\n")

  test("parse1") {
    parse(f)
  }

  test("self-parse1") {
    val me = parse(f).get.show
    assertEquals(parse(me).get, parse(f).get)
  }
}
