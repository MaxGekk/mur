package mur

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

trait Parsers extends RegexParsers {

  def expr:Parser[Expr] = """\d+""".r ^^ {
    s => Literal(s.toInt)
  }
}

object Parsers extends Parsers {

  def parseExpression(s: CharSequence): Expr = {
    parseExpression(new CharSequenceReader(s))
  }

  def parseExpression(input: CharSequenceReader): Expr = {
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
