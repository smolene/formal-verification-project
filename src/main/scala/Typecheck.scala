package typecheck

import termsAndTypes._
import Name._
import error._

def uniqueProgram(p: Program): (Program, Map[NameS, DefFun], Map[NameS, DefType]) =
  val (ntypes, tglob, consMap) = uniqueVarsDefType(p.types)
  val (nfns, fglob) = uniqueVarsDefFun(p.fns, tglob, consMap)
  (Program(ntypes ++ nfns), fglob, tglob)

def uniqueVarsDefFun(fls: List[DefFun], tglob: Map[NameS, DefType], 
  consMap: Map[NameS, Map[NameS, NameU]]): (List[DefFun], Map[NameS, DefFun]) =
  val renamed = fls.map { df => (df.n.asS, df.copy(n = df.n.freshU)) }
  val onlyNameRenamed = renamed.toMap
  val res = renamed.map(_._2).map { df =>
    val ntargs = df.targs.map(arg => (arg.asS, arg.freshU))
    val ntpe = uniqueVars(df.tpe)(using ntargs.toMap, tglob)
    val nbody = uniqueVars(df.body)(using Map(), onlyNameRenamed, tglob, consMap)
    DefFun(df.n, ntargs.map(_._2), ntpe, nbody)
  }
  val fglob = renamed.map(_._1).zip(res).toMap
  (res, fglob)

def uniqueVarsDefType(tls: List[DefType]): 
  (List[DefType], Map[NameS, DefType], Map[NameS, Map[NameS, NameU]]) =
  // rename first to allow cons arguments to refer to other types
  val renamed = tls.map { dt => (dt.n.asS, dt.copy(n = dt.n.freshU)) }
  val onlyNameRenamed = renamed.toMap
  val (res, consList) = renamed.map(_._2).map { dt =>
    // TODO: is this correct? i think yes but yk
    val ntargs = dt.targs.map(n => (n.asS, n.freshU))
    val ncons = dt.cons.map { (n, args) =>
      val nn = n.freshU
      val nargs = args.map { arg => uniqueVars(arg)(using ntargs.toMap, onlyNameRenamed) }
      (nn, nargs)
    }
    val consNameMap = dt.cons.zip(ncons).map((s, u) => (s._1.asS, u._1))
    (DefType(dt.n, ntargs.map(_._2), ncons), consNameMap)
  }.unzip
  val consMap = renamed.map(_._1).zip(consList)
    .map((ns, conss) => (ns, conss.toMap)).toMap
  //val dtMap = res.map(dt => (dt.n, dt)).toMap
  val tglob = renamed.map(_._1).zip(res).toMap
  (res, tglob, consMap)

def uniqueVars(t: Term)(using 
  m: Map[NameS, NameU], 
  glob: Map[NameS, DefFun],
  tglob: Map[NameS, DefType],
  consMap: Map[NameS, Map[NameS, NameU]],
): Term = t match
  case Var(n) => 
    val ns = n.asS
    Var(m.get(ns)
      .orElse(glob.get(ns).map(_.n))
      .orElse(consMap.find((tn, conss) => conss.contains(ns)).map(_._2(ns)))
      .getOrElse(throw CompileError(s"cannot find variable $n in $m and $glob and $consMap"))
    )
  case App(f, arg) => App(uniqueVars(f), uniqueVars(arg))
  case TApp(Var(n), targs) =>
    val fn = glob.getOrElse(
      n.asS, 
      throw CompileError(s"cannot find global function $n")
    )
    if fn.targs.length != targs.length then
      throw CompileError(s"wrong number of arguments for fn $fn. targs: $targs")
    val ntargs = targs.map(uniqueVars)
    TApp(Var(fn.n), ntargs)
  case TApp(f, targs) => 
    throw CompileError(s"tapp only allowed on global functions $t")
  case Match(scrut, cs) =>
    val ncs = cs.map { (p, t) =>
      val (np, nm) = uniqueVars(p)
      (np, uniqueVars(t)(using m ++ nm))
    }
    Match(uniqueVars(scrut), ncs)
  case Lambda(xs, xt, body) => 
    val nxs = xs.freshU
    val nxt = uniqueVars(xt)
    val nbody = uniqueVars(body)(using m + (xs.asS -> nxs))
    Lambda(nxs, nxt, nbody)

def uniqueVars(p: Pattern)
  (using tglob: Map[NameS, DefType], consMap: Map[NameS, Map[NameS, NameU]]): 
  (Pattern, Map[NameS, NameU]) = p match
  case IdPattern(n) =>
    val nn = n.freshU
    (IdPattern(nn), Map(n.asS -> nn))
  case Wildcard() => (p, Map())
  case ConsPattern(consx, args) =>
    def error = throw CompileError(s"cannot find constructor $consx in\n$tglob\n$consMap")
    val cons = consx.asS
    val (nt, ncons) = consMap.find((tn, tm) => tm.contains(cons))
      .map((tn, tm) => (tn, tm(cons))).getOrElse(error)
    val consArgs = tglob.getOrElse(nt, error).cons
      .find((n, _) => n == ncons).map(_._2).getOrElse(error)

    if consArgs.length != args.length then
      throw CompileError(s"wrong number of arguments in $p. cons: $ncons, args: $consArgs")

    val (nargs, nmaps) = args.map(uniqueVars).unzip
    (ConsPattern(ncons, nargs), nmaps.foldLeft(Map[NameS, NameU]())(_ ++ _))

def uniqueVars(t: Type)(using tvars: Map[NameS, NameU], tglob: Map[NameS, DefType]): Type = t match
  case TCons(t, Nil) if tvars.contains(t.asS) => ???
  case TCons(t, targs) => 
    val tt = tglob.getOrElse(t.asS, throw CompileError(s"cannot find type $t in $tglob"))
    if targs.length != tt.targs.length then
      throw CompileError(s"type applied to the wrong number of arguments." +
        s"got $targs, expected ${tt.targs} in $tt")
      // !!! here tt.targs is NOT updated, it's not uniqueVared yet
    val ntargs = targs.map(uniqueVars)
    TCons(tt.n, ntargs)
  case TFn(t, u) => TFn(uniqueVars(t), uniqueVars(u))
  case TVar(t) => ??? // should not have any at this step of compilation
  case TUk() => throw CompileError(s"type inference not implemented yet: $t")
