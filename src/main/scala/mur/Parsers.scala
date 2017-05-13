package mur

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.JavaTokenParsers

trait Parsers extends RegexParsers with JavaTokenParsers {

  def num: Parser[Literal] = floatingPointNumber ^^ { s =>
    Try{ Literal(s.toInt) }.recover { case _ => Literal(s.toDouble) }.get
  }

  def id: Parser[Id] = ident ^^ Id

  def brackets: Parser[Brackets] = "(" ~> expr <~ ")" ^^ {
    case (e) => Brackets(e)
  }

  def operand = (num | brackets | id)

  def pow = chainl1(operand, "^" ^^^ Pow)
  def term = chainl1(pow, "*" ^^^ Mul | "/" ^^^ Div)

  def sequence: Parser[Sequence] = "{" ~> expr ~ "," ~ expr <~ "}" ^^ {
    case (begin ~ "," ~ end) => Sequence(begin, end)
  }

  def expr = chainl1(term, "+" ^^^ Plus | "-" ^^^ Minus) | sequence

  def map: Parser[MapSeq] = "map(" ~> expr ~ "," ~ ident ~ "->" ~ expr <~ ")" ^^ {
    case (s ~ _ ~ i ~ _ ~ e) => MapSeq(s, Id(i), e)
  }

  def mrexpr = (map | expr)

  def out: Parser[Out] = "out" ~> mrexpr ^^ Out
  def print: Parser[Print] = "print" ~> stringLiteral ^^ {
    case (s) => Print(s.substring(1, s.size - 1))
  }
  def vardef: Parser[VarDef] = "var" ~ ident ~ "=" ~ mrexpr ^^ {
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
