package mur

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.JavaTokenParsers

// Parser-combinators for our MuR language
trait Parsers extends RegexParsers with JavaTokenParsers {
  // Catch a floating-point number (positive or negative) and try to
  // convert it to an integer first of all otherwise to double
  def num: Parser[Literal] = positioned { floatingPointNumber ^^ { s =>
    Try{ Literal(s.toInt) }.recover { case _ => Literal(s.toDouble) }.get
  }}
  // Parser for standard Java identifier
  def id: Parser[Id] = positioned { ident ^^ Id }
  // Parser for round brackets: ( 1 + 2 )
  def brackets: Parser[Brackets] = positioned {"(" ~> expr <~ ")" ^^ {
    case (e) => Brackets(e)
  }}
  // Operands of arithmetic operations: ^, *, /, +, -
  def operand = positioned(map | reduce | num | brackets | id
    | "" ~> failure("expected operands: map, reduce, number, (), identifier"))

  def pow = positioned(chainl1(operand, "^" ^^^ Pow))
  def term = positioned(chainl1(pow, "*" ^^^ Mul | "/" ^^^ Div))
  // Parser for the expression like this {1, 10} which represents
  // the sequence of numbers 1,2,3,4,5,6,7,8,9,10
  def sequence: Parser[Sequence] = positioned { "{" ~> expr ~ "," ~ expr <~ "}" ^^ {
    case (begin ~ "," ~ end) => Sequence(begin, end)
  }}

  def expr = positioned(chainl1(term, "+" ^^^ Plus | "-" ^^^ Minus) | sequence
    | "" ~> failure("incorrect expression"))

  // Parser of the map operator: map({0, n}, i -> i + 1)
  def map: Parser[MapSeq] = positioned {
    "map" ~> "(" ~> expr ~ "," ~ ident ~ "->" ~ expr <~ ")" ^^ {
      case (s ~ _ ~ i ~ _ ~ e) => MapSeq(s, Id(i), e)
    }
  }
  // Parser for the reduce operator: reduce({1, 10}, 0, i j -> i + j)
  def reduce: Parser[ReduceSeq] = positioned {
    "reduce" ~> "(" ~> expr ~ "," ~ expr ~ "," ~ ident ~ ident ~ "->" ~ expr <~ ")" ^^ {
      case (s ~ _ ~ ini ~ _ ~ x ~ y ~ _ ~ e) => ReduceSeq(s, ini, Id(x), Id(y), e)
    }
  }
  // All expressions that can be parser and calculated
  def mrexpr = positioned(map | reduce | expr
    | "" ~> failure("expected map/reduce or an expression"))
  // Parser for output of any expression: out 1 + 2*3
  def out: Parser[Out] = positioned { "out" ~> mrexpr ^^ Out }
  // Parser of print. stringLiteral recognises a string with ""
  // that's why we have to remove them
  def print: Parser[Print] = positioned { "print" ~> stringLiteral ^^ {
    case (s) => Print(s.substring(1, s.size - 1))
  }}
  // Parser for variable definition like var n = 500
  def vardef: Parser[VarDef] = positioned { "var" ~ ident ~ "=" ~ mrexpr ^^ {
    case (_ ~ i ~ _ ~ e) => VarDef(i, e)
  }}

  def stmt: Parser[Stmt] = positioned(out | print | vardef
    | "" ~> failure("expected statements: out, print or var"))
  // Program is just repeated statements
  def prog = rep(stmt) ^^ {Program(_)}
}

object Parsers extends Parsers {
  /**
    * Parsing of a text of a program to its intermediate representation (IR)
    * @param s - sequence of chars represented the program
    * @return - either IR of the program nor an error
    */
  def parse(s: CharSequence): Either[mur.Error, Program] = {
    parse(new CharSequenceReader(s)) match {
      case Success(res, _) => Right(res)
      case NoSuccess(msg, next) =>
        Left(mur.Error(pos = next.pos, msg))
    }
  }
  def parse(input: CharSequenceReader): ParseResult[Program] = parsePhrase(input)

  def parsePhrase(input: CharSequenceReader): ParseResult[Program] = {
    phrase(prog)(input)
  }
}
