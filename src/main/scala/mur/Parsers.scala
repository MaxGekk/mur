package mur

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.JavaTokenParsers

trait Parsers extends RegexParsers with JavaTokenParsers {

  def num: Parser[Literal] = floatingPointNumber ^^ { s =>
    Try{ Literal(s.toInt) }.recover { case _ => Literal(s.toDouble) }.get
  }

  def id: Parser[Id] = ident ^^ { name => Id(name)}

  def brackets: Parser[Brackets] = "(" ~ expr ~ ")" ^^ {
    case ("(" ~ e ~ ")") => Brackets(e)
  }

  def operand = (num | brackets | id)

  def plus:Parser[Plus] = operand ~ "+" ~ operand ^^ {
    case (x ~ "+" ~ y) => Plus(x, y)
  }
  def minus:Parser[Minus] = operand ~ "-" ~ operand ^^ {
    case (x ~ "-" ~ y) => Minus(x, y)
  }
  def mul:Parser[Mul] = operand ~ "*" ~ operand ^^ {
    case (x ~ "*" ~ y) => Mul(x, y)
  }
  def div:Parser[Div] = operand ~ "/" ~ operand ^^ {
    case (x ~ "/" ~ y) => Div(x, y)
  }
  def pow:Parser[Pow] = operand ~ "^" ~ operand ^^ {
    case (x ~ "^" ~ y) => Pow(x, y)
  }
  def op: Parser[Expr] = (plus | minus | mul | div | pow)
  def sequence: Parser[Sequence] = "{" ~ expr ~ "," ~ expr ~ "}" ^^ {
    case ("{" ~ begin ~ "," ~ end ~ "}") => Sequence(begin, end)
  }
  def map: Parser[MapSeq] = "map" ~ "(" ~ expr ~ "," ~ id ~ "->" ~ expr ~ ")" ^^ {
    case ("map" ~ "(" ~ seq ~ "," ~ ident ~ "->" ~ e ~ ")") => MapSeq(seq, ident, e)
  }
  def reduce: Parser[ReduceSeq] = {
    "reduce" ~ "(" ~ expr ~ "," ~ expr ~ "," ~ id ~ id ~ "->" ~ expr ~ ")" ^^ {
      case ("reduce" ~ "(" ~ seq ~ "," ~ init ~ "," ~ x ~ y ~ "->" ~ e ~ ")") =>
        ReduceSeq(seq, init, x, y, e)
    }
  }

  def expr:Parser[Expr] = (op | num | brackets | id | sequence | map | reduce)

  def out: Parser[Out] = "out" ~ expr ^^ {
    case ("out" ~ e ) => Out(e)
  }
  def print: Parser[Print] = "print" ~ stringLiteral ^^ {
    case ("print" ~ s) => Print(s.substring(1, s.size - 1))
  }
  def vardef: Parser[VarDef] = "var" ~ ident ~ "=" ~ expr ^^ {
    case (_ ~ i ~ _ ~ e) => VarDef(i, e)
  }

  def stmt: Parser[Stmt] = (out | print | vardef)
}

object Parsers extends Parsers {

  def parse(s: CharSequence): Stmt = {
    parse(new CharSequenceReader(s))
  }

  def parse(input: CharSequenceReader): Stmt = {
    parsePhrase(input) match {
      case Success(t, _) => t
      case NoSuccess(msg, next) => throw new IllegalArgumentException(
        "Could not parse '" + input + "' near '" + next.pos.longString + ": " + msg)
    }
  }

  def parsePhrase(input: CharSequenceReader): ParseResult[Stmt] = {
    phrase(stmt)(input)
  }
}
