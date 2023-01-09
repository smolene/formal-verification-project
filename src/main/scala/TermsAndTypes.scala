package termsAndTypes

import error._
import scala.util.parsing.input.Positional

extension (s: String)
  def indent(i: Int): String = 
    s.split('\n').mkString(" " * i*2, "\n" + " " * i*2, "")

sealed trait Printable:
  //override def toString() = show
  // please god, may i never have to lay eyes upon this ever again
  def show: String = this match
    case Program(items) => items.map(_.show).mkString("\n")
    case Name(n) => n.s
    case IdPattern(n) => n.n.s
    case Wildcard() => "_"
    case ConsPattern(cons, args) => s"${cons.show}(${args.map(_.show).mkString(", ")})"
    case DefFun(n, targs, tpe, body) =>
      val targss = if targs.isEmpty 
        then ""
        else s"[${targs.map(_.show).mkString(", ")}]"
      s"def ${n.show}$targss: ${tpe.show} = {\n${body.show.indent(1)}\n}"
    case DefType(n, targs, cons) =>
      val targss = if targs.isEmpty 
        then ""
        else s"[${targs.map(_.show).mkString(", ")}]"
          
      val conss = cons.map { (n, ts) =>
        s"case ${n.show}(${ts.map(_.show).mkString(", ")})"
      }.mkString("\n", "\n", "").indent(1)

      s"type ${n.show}$targss {$conss\n}"
    case Var(n) => n.n.s
    case App(f, arg) => f match
      case Lambda(xs, xt, body) =>
        s"let ${xs.show}: ${xt.show} = ${arg.show} in\n${body.show}"
      case App(_, _) => s"(${f.show})(${arg.show})"
      case _ => s"${f.show}(${arg.show})"
    case TApp(f, targs) => s"${f.show}[${targs.map(_.show).mkString(", ")}]"
    case Match(scrut, cs) =>
      val cases = cs.map { (pat, body) =>
        s"case ${pat.show} => ${body.show}"
      }.mkString("\n", "\n", "").indent(1)
      s"${scrut.show} match {$cases\n}"
    case Lambda(xs, xt, body) => 
      s"(\\${xs.show}: ${xt.show}. \n${body.show.indent(1)})"
    case TCons(t, targs) => if targs.isEmpty 
      then t.show
      else s"${t.show}${targs.map(_.show).mkString("[", ", ", "]")}"
    case TVar(t) => t.n.s
    case TFn(t, u) => s"(${t.show} -> ${u.show})"
    case TUk() => "?"

enum NName(val s: String):
  case Str(override val s: String) extends NName(s)
  case Uni(name: String, i: Int) extends NName(s"$name$i")

import Name._

case class Name[N <: NName](n: N) extends Printable:
  override def toString(): String = n.s
        
  def asS: NameS = n match
    case s: NName.Str => Name(s)
    case _: NName.Uni => throw CompileError(s"name is already U: $n")
  
  def freshU: NameU = n match
    case s: NName.Str => Name.fresh(Name(s))
    case _: NName.Uni => throw CompileError(s"name is already U: $n")

object Name:
  type NameS = Name[NName.Str]
  type NameU = Name[NName.Uni]
  type NameX = Name[? <: NName]
  def name(s: String): NameS = Name(NName.Str(s))
  var counter = 0
  def fresh(n: Name[NName.Str]): Name[NName.Uni] = 
    val res: Name[NName.Uni] = Name(NName.Uni(n.n.s, counter))
    counter += 1
    res

case class Program(items: List[Item]) extends Positional with Printable:
  def fns = items.collect { case f: DefFun => f }
  def types = items.collect { case t: DefType => t }
  lazy val funMap = fns.map(fn => (fn.n, fn)).toMap
  lazy val typMap = types.map(tpe => (tpe.n, tpe)).toMap
  lazy val consMap = types.flatMap(tpe => tpe.cons.map((tpe, _))).map {
    case (dt, (consn, consargs)) => (consn, (dt, consargs))
  }.toMap

sealed trait Item extends Positional with Printable
case class DefFun(
  n: NameX, 
  targs: List[NameX], 
  tpe: Type, 
  body: Term,
) extends Item
case class DefType(
  n: NameX, 
  targs: List[NameX],
  cons: List[(NameX, List[Type])],
) extends Item

sealed trait ID(idprefix: String):
  lazy val id = ID.makeId
  lazy val idString = s"$idprefix$id"

object ID:
  var counter = 0
  def makeId: Int =
    val res = counter
    counter += 1
    res

sealed trait Term extends Positional with Printable with ID
case class Var(n: NameX) extends Term with ID("v")
case class App(f: Term, arg: Term) extends Term with ID("app")
case class TApp(f: Term, targs: List[Type]) extends Term with ID("tapp"):
  require(!targs.isEmpty)
case class Match(scrut: Term, cs: List[(Pattern, Term)]) extends Term with ID("matc")
case class Lambda(xs: NameX, xt: Type, body: Term) extends Term with ID("lbd")

sealed trait Pattern extends Positional with Printable with ID
case class IdPattern(n: NameX) extends Pattern with ID("idpat")
case class Wildcard() extends Pattern with ID("wild")
case class ConsPattern(cons: NameX, args: List[Pattern]) extends Pattern with ID("conspat")

sealed trait Type extends Positional with Printable with ID:
  def isDef = this match
    case _: TCons | _: TVar | _: TFn => true
    case _: TUk => false
  
case class TCons(t: NameX, targs: List[Type]) extends Type with ID("tc")
// Everything parses as TCons. If a TCons is actually a type var,
// it is converted to a TVar when name analysing.
case class TVar(t: NameX) extends Type with ID("tv")
case class TFn(t: Type, u: Type) extends Type with ID("tfn")
case class TUk() extends Type with ID("tuk")
