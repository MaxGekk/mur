package mur

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

trait Parsers extends RegexParsers {

  def integer: Parser[Literal] = """-?\d+""".r ^^ {
    s => Literal(s.toInt)
  }
  def brackets = "(" ~> expr <~ ")"

  def operand = (integer | brackets)

  def plus:Parser[Plus] = operand ~ "+" ~ operand ^^ {
    case (x ~ "+" ~ y) => Plus(x, y)
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

  def expr:Parser[Expr] = (plus | mul | div | pow | integer | brackets)
}

object Parsers extends Parsers {

  def parseExpr(s: CharSequence): Expr = {
    parseExpr(new CharSequenceReader(s))
  }

  def parseExpr(input: CharSequenceReader): Expr = {
    parsePhrase(input) match {
      case Success(t, _) => t
      case NoSuccess(msg, next) => throw new IllegalArgumentException(
        "Could not parse '" + input + "' near '" + next.pos.longString + ": " + msg)
    }
  }

  def parsePhrase(input: CharSequenceReader): ParseResult[Expr] = {
    phrase(expr)(input)
  }
}
