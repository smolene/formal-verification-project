package logic

import termsAndTypes._
import Name._
import error._

sealed trait Dl:
  def withHeader: String = 
    import scala.io.Source
    val f = Source.fromFile("src/main/datalog/header.dlog")
    f.getLines().mkString("", "\n", "\n\n") ++ this.show

  def show: String = this match
    case Ident(s) => s
    case Struct(n, args) => s"${n.show}(${args.map(_.show).mkString(",")})"
    case Implies(l, r) => if r.isEmpty
      then s"${l.show}."
      else s"${l.show} :- ${r.map(_.show).mkString(", ")}."
    case Query(q) => s"${q.show}?"
    case Packed(elems) => elems.map(_.show).filter(!_.isEmpty).mkString("\n")
    case Spaced(elems) => elems.map(_.show).filter(!_.isEmpty).mkString("\n\n")

case class Ident(s: String) extends Dl
object Ident:
  def apply(n: NameX): Ident = n.n match
    case _: NName.Uni => Ident(s"_${n.n.s}")
    case _ => throw CompileError(s"s name should never be here!! $n")

given Conversion[String, Ident] with
  def apply(s: String): Ident = Ident(s)

case class Struct(n: Ident, args: List[Ident]) extends Dl
object Struct:
  def apply(n: Ident, args: Ident*): Struct = Struct(n, args.toList)

case class Implies(l: Struct, r: List[Struct]) extends Dl
object Implies:
  def apply(l: Struct, r: Struct*): Implies = Implies(l, r.toList)

case class Query(q: Struct) extends Dl

case class Packed(elems: List[Dl]) extends Dl
object Packed:
  def apply(elems: Dl*): Packed = Packed(elems.toList)
case class Spaced(elems: List[Dl]) extends Dl
object Spaced:
  def apply(elems: Dl*): Spaced = Spaced(elems.toList)

object DlConst:
  def makeList[A](ls: List[A], s: String): List[Ident] =
    ls.zipWithIndex.map((_, i) => s"$s$i")

  def ascribedIdent(id: Ident) = Ident(s"${id.s}_ascribed")

  object Defs:
    val typ = Ident("type")
    val typed = Ident("typed")
    val type_constr = Ident("type_constr")
    val fun = Ident("fun")
    val lambda = Ident("lambda")
    val app = Ident("app")
    val var_ = Ident("var")
    val match_ = Ident("match")
    val pattern_match_list = Ident("pattern_match_list")

import DlConst.Defs._

def toDatalog(p: Program): Dl = 
  val (fndl, queries) = p.items.map(i => toDatalog(i)(using p)).unzip
  Spaced(Spaced(fndl), Packed(queries))

// returns the function and the query, queries have to be put at the very end
def toDatalog(i: Item)(using p: Program): (Dl, Dl) = i match
  case f: DefFun  => toDatalog(f)
  case t: DefType => (toDatalog(t), Packed())

def toDatalog(t: DefType): Dl =
  import DlConst.makeList
  if t.targs.isEmpty then
    val tn = Ident(t.n)
    val s1 = Implies(Struct(typ, tn))
    val consSigs = t.cons.map { (cn, cargs) => 
      val fn = cargs.foldRight[Type](TCons(t.n, Nil)) { (arg, acc) => 
        TFn(arg, acc)
      }
      val (fndl, fnid) = toDatalog(fn)
      val typedDl = Implies(Struct(
        typed,
        Ident(cn),
        fnid,
      ))
      Packed(fndl, typedDl)
    }
    
    val consRules = t.cons.map { (cn, cargs) =>
      val cs = makeList(cargs, "A").zip(cargs).map { case (id, tpe) =>
        val sn = tpe match
          case TCons(t, Nil) => Ident(t)
          case _ => ???
        
        Struct(typed, id, sn)
      }
      val typeImpl = Implies(
        Struct(typed, "E", tn),
        Struct(Ident(cn), Ident("E") :: makeList(cargs, "A")) :: cs
      )

      typeImpl
    }

    val consConstraints = t.cons.map { (cn, cargs) =>
      val argsConstr = cargs.zipWithIndex.map { (arg, i) => 
        val x = Ident("X")
        val (tdl, tid) = toDatalog(arg)
        val vars = (0 until cargs.length).map(i => Ident(s"E$i"))
        val dl = Implies(Struct(
          type_constr,
          x,
          tid,
        ), Struct(
          Ident(cn),
          Ident("E") :: vars.updated(i, x).toList
        ))
        Packed(tdl, dl)
      }
      Packed(argsConstr)
    }

    Spaced(
      Packed(s1 :: consSigs) :: 
      Packed(consRules) :: 
      Packed(consConstraints) :: 
      Nil
    )
  else
    ???
  
// returns the function and the query, queries have to be put at the very end
def toDatalog(f: DefFun)(using p: Program): (Dl, Dl) =
  if f.targs.isEmpty then
    val id = Ident(f.n)

    val (bodydl, ident) = toDatalog(f.body, Some(id))
    assert(ident == id)

    val (retdl, retid) = toDatalog(f.tpe)
    val retquery = Query(Struct(
      typed,
      id,
      retid,
    ))

    val ascribedid = DlConst.ascribedIdent(id)
    val ascribed = Implies(Struct(
      typed,
      ascribedid,
      retid,
    ))

    (Packed(bodydl, retdl, ascribed), Packed(retquery))
  else
    ???

def toDatalog(t: Term, toplevel: Option[Ident] = None)
  (using p: Program): (Dl, Ident) = 
  val id = toplevel.getOrElse(Ident(t.idString)) 
  t match
    case Var(n) =>
      p.funMap.get(n) match
        case Some(fn) => 
          (Packed(), DlConst.ascribedIdent(Ident(n)))
        case _ => (Packed(), Ident(n))
    case App(f, arg) =>
      val (fdl, fid) = toDatalog(f)
      val (argdl, argid) = toDatalog(arg)
      val dl = Implies(Struct(
        app,
        id,
        fid,
        argid,
      ))
      (Packed(fdl, argdl, dl), id)
    case TApp(f, targs) => throw CompileError("TApp unsupported yet")
    case Match(scrut, cs) => 
      val (scrutdl, scrutid) = toDatalog(scrut)
      val ncs = cs.map { (pat, b) =>
        val (patdl, patid) = toDatalog(pat)
        val (bdl, bid) = toDatalog(b)
        val mid = Ident(b.idString)
        val mdl = Implies(Struct(
          match_,
          mid,
          scrutid,
          patid,
          bid,
        ))
        (Packed(patdl, mdl, bdl), mid)
      }
      val (patmatls, patmatid) = ncs.init.foldRight((Packed(), ncs.last._2)) { 
        case ((ldl, lid), (dl, id)) =>
          val nid = Ident(s"ls_${lid.s}")
          val ndl = Implies(Struct(
            pattern_match_list,
            nid,
            id,
            lid,
          ))
          (Packed(dl, ndl), nid)
      }
      (Packed(scrutdl +: ncs.map(_._1) :+ patmatls), patmatid)
    case Lambda(xs, xt, body) =>
      val vid = Ident(xs)
      val vdl = Implies(Struct(
        var_,
        vid,
      ))
      val (tdl, tid) = toDatalog(xt)

      val typedv = Implies(Struct(
        typed,
        vid,
        tid,
      ))

      val (bodydl, bodyid) = toDatalog(body)

      val dl = Implies(Struct(
        lambda,
        id,
        vid,
        bodyid,
      ))

      (Packed(bodydl, tdl, vdl, typedv, dl), id)

def toDatalog(p: Pattern): (Dl, Ident) = p match
  case Wildcard() => 
    toDatalog(IdPattern(name(s"wildcard_${p.idString}").freshU))
  case IdPattern(n) =>
    val nid = Ident(n)
    val ndl = Implies(Struct(var_, nid))
    val grandT = Ident("T")
    val typedv = Implies(Struct(
      typed,
      nid,
      grandT,
    ), Struct(
      type_constr,
      nid,
      grandT,
    ))
    //typed(X,T) :- var(X), type_constr(X,T).
    (Packed(ndl, typedv), nid)
  case ConsPattern(cons, args) =>
    val nargs = args.foldRight(List[(Dl, Ident)]()) { (p, acc) =>
      val (pdl, pid) = toDatalog(p)
      (pdl, pid) :: acc
    }
    val id = Ident(s"pat_${p.idString}")
    val dl = Implies(Struct(
      Ident(cons),
      id :: nargs.map(_._2)
    ))
    (Packed(nargs.map(_._1) :+ dl), id)

def toDatalog(t: Type): (Dl, Ident) = t match
  case TCons(t, targs) =>
    if !targs.isEmpty then
      throw CompileError("targs unsupported yet")
    // Do nothing: the type(t) should already be defined somewhere in type definition.
    // note that defining it here would be a mistake is it will introduce
    // a bogus unknown type as if it were an actual type
    val id = Ident(t)
    //(Implies(Struct(id)), id)
    (Packed(), id)
  case fn@TFn(t, u) => 
    val (tdl, tid) = toDatalog(t)
    val (udl, uid) = toDatalog(u)
    val n = Ident(fn.idString)
    val dl = Packed(
      tdl,
      udl,
      Implies(Struct(fun, n, tid, uid)),
    )
    (dl, n)
  case TVar(t) => throw CompileError("tvars unsupported yet")
  case TUk() => throw CompileError("unknown type")

