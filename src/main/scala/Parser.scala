package parser

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import termsAndTypes.{given, _}
import Name._

def parse[T](s: String, p: Parser.Parser[T] = Parser.program): Parser.ParseResult[T] =
  val tokens = Parser.lexical.Scanner(s)
  Parser.phrase(p)(tokens)

def parseItem(s: String) = parse(s, Parser.item)

def parseTerm(s: String) = parse(s, Parser.term)

def parseType(s: String) = parse(s, Parser.typ)

object Parser extends StandardTokenParsers {

  lexical.delimiters ++= List("(", ")", "[", "]", "=", "=>", "->", "{", "}", ",", ":", "\\", ".", "?")
  lexical.reserved ++= List("match", "let", "in", "def", "type", "case")

  def program: Parser[Program] =
    rep(item) ^^ (is => Program(is))

  def item: Parser[Item] =
    "def" ~ ident ~ 
      opt("[" ~ rep1sep(ident, ",") ~ opt(",") ~ "]") ~
      opt("(" ~ rep1sep(ident ~ opt(":" ~> typ), ",") ~ opt(",") ~ ")") ~
      opt(":" ~> typ) ~ "=" ~ "{" ~ term ~ "}" ^^ {
      case _ ~ defName ~
        typOpt ~
        arglist ~
        retOpt ~ _ ~ _ ~ body ~ _ =>
        
        val ntypArgs = typOpt.map { case _ ~ typs ~ _ ~ _ =>
          typs.map(name(_))
        }.getOrElse(Nil)

        arglist match
          case None =>
            DefFun(
              name(defName),
              ntypArgs,
              retOpt.getOrElse(TUk()),
              body,
            )
          case Some(_ ~ args ~ _ ~ _) =>
            val ntyp = args.map {
              case id ~ ot => ot.getOrElse(TUk())
            }.appended(retOpt.getOrElse(TUk()))
            .reduceRight { (l, r) => TFn(l, r) }

            //val nbody = Lambda(args.map { 
            //  case id ~ ot => (name(id), ot.getOrElse(TUk()))
            //}, body)

            val nbody = args.foldRight(body) { case (an ~ at, b) => 
              Lambda(name(an), at.getOrElse(TUk()), b) 
            }

            DefFun(
              name(defName), 
              ntypArgs,
              ntyp,
              nbody,
            )
    } |
    "type" ~ ident ~ 
      opt("[" ~ rep1sep(ident, ",") ~ opt(",") ~ "]") ~ "{" ~
      rep("case" ~ ident ~ "(" ~
        repsep(typ, ",") ~ opt(",") ~
      ")") ~ "}" ^^ {
      case _ ~ typName ~ typArgsOpt ~ _ ~ cases ~ _ =>
        DefType(
          name(typName),
          typArgsOpt.map { case _ ~ ids ~ _ ~ _ =>
            ids.map(name(_))
          }.getOrElse(Nil),
          cases.map { case _ ~ c ~ _ ~ ids ~ _ ~ _ => 
            (name(c), ids)  
          }
        )
    }

  def term: Parser[Term] =
    "let" ~ ident ~ opt(":" ~> typ) ~ "=" ~ term ~ "in" ~ term ^^ {
      case _ ~ id ~ typOpt ~ _ ~ lhs ~ _ ~ body =>
        App(Lambda(name(id), typOpt.getOrElse(TUk()), body), lhs)
    } |
    "\\" ~ rep1sep(ident ~ opt(":" ~> typ), ",") ~ opt(",") ~ "." ~ term ^^  {
      case _ ~ args ~ _ ~ _ ~ body =>
        args.foldRight(body) { case (an ~ at, b) => 
          Lambda(name(an), at.getOrElse((TUk())), b)  
        }
    } |
    appTerm ~ rep(matc) ^^ {
      case ter ~ matcs => matcs.foldLeft(ter) { (acc, matc) =>
        Match(acc, matc)
      }
    }

  def matc: Parser[List[(Pattern, Term)]] =
    "match" ~ "{" ~ rep("case" ~ pattern ~ "=>" ~ term) ~ "}" ^^ {
      case _ ~ _ ~ cases ~ _ => cases.map {
        case _ ~ pat ~ _ ~ ter => (pat, ter)
      }
    }

  def pattern: Parser[Pattern] =
    "_" ^^^ Wildcard() |
    ident ~ opt("(" ~ repsep(pattern, ",") ~ opt(",") ~ ")") ^^ {
      case id ~ None => IdPattern(name(id))
      case id ~ Some(_ ~ patterns ~ _ ~ _) => 
        ConsPattern(name(id), patterns)
    }

  def appTerm: Parser[Term] = 
    tappTerm ~ "(" ~ repsep(term, ",") ~ opt(",") ~ ")" ^^ {
      case ter ~ _ ~ args ~ _ ~ _ => args.foldLeft(ter) { (t, a) => 
        App(t, a)
      }
    } |
    tappTerm

  def tappTerm: Parser[Term] =
    simpleTerm ~ "[" ~ rep1sep(typ, ",") ~ opt(",") ~ "]" ^^ {
      case ter ~ _ ~ targs ~ _ ~ _ => TApp(ter, targs)
    } |
    simpleTerm

  def simpleTerm: Parser[Term] =
    ident ^^ (s => Var(name(s))) |
    "(" ~> term <~ ")"

  def typ: Parser[Type] =
    simpleTyp ~ "->" ~ typ ^^ {
      case t ~ _ ~ u => TFn(t, u)
    } | simpleTyp

  def simpleTyp: Parser[Type] =
    "?" ^^^ TUk() |
    ident ~ opt("[" ~ rep1sep(typ, ",") ~ opt(",") ~ "]") ^^ {
      case typ ~ None => TCons(name(typ), Nil)
      case typ ~ Some(_ ~ args ~ _ ~ _) => TCons(name(typ), args)
    } |
    "(" ~> typ <~ ")"

}
