package gavin.parser

import scala.util.parsing.combinator._

/**
 * EBNF form for parsing keyword rule
 *
 * Operator priority
 * ! > and > or
  *
 * AST result examples)
 *
 * (청바지 and !면바지) or !레이스
 * -> Or(And(Keyword(청바지),Not(Keyword(면바지))),Not(Keyword(레이스)))
 *
 * !(청바지 and !면바지) or 레이스
 * -> Or(Not(And(Keyword(청바지),Not(Keyword(면바지)))),Keyword(레이스))
 *
 * 청바지 and !(!면바지 or 레이스)
 * -> And(Keyword(청바지),Not(Or(Not(Keyword(면바지)),Keyword(레이스))))
 */
class SimpleKeywordRuleParserEBNF extends RegexParsers {

  def top_expr: Parser[ExprSymbol]    = or_expr

  def or_expr: Parser[ExprSymbol]     = and_expr ~ ("or" ~ and_expr).* ^^ {
    case left ~ list => list.foldLeft(left) {
      case (left, "or" ~ right) => Or(left, right)
    }
  }
  def and_expr: Parser[ExprSymbol]    = not_expr ~ ("and" ~ not_expr).* ^^ {
    case left ~ list => list.foldLeft(left) {
      case (left, "and" ~ right) => And(left, right)
    }
  }
  def not_expr: Parser[ExprSymbol]    = (
    "!" ~ bracket_expr                        ^^ { case "!" ~ list => Not(list) }
      | bracket_expr                          ^^ (list => list)
    )
  def bracket_expr: Parser[ExprSymbol]        = (
    "(" ~ or_expr ~ ")"                       ^^ { case "(" ~ list ~ ")" => list }
      | keyword                               ^^ { keyword: Keyword => keyword }
    )

  def keyword: Parser[Keyword]        = (
    "\"" ~ "[^\"]+".r ~ "\""                  ^^ { case "\"" ~ value ~ "\"" => Keyword(value) }
      | "[^ ()!]+".r                          ^^ (value => Keyword(value))
    )

  def parseAST(targetStr: String): ExprSymbol = {
    parseAll(top_expr, targetStr) match {
      case Success(result, _) =>
        result
      case Failure(msg, _) =>
        throw SimpleKeywordRuleParseException(s"Parsing failed: $targetStr -> $msg")
      case Error(msg, _) =>
        throw SimpleKeywordRuleParseException(s"Parsing error: $targetStr -> $msg")
    }
  }

}

/**
 * case classes to build abstract syntax tree (AST)
 */
sealed abstract class ExprSymbol
case class Keyword(term: String) extends ExprSymbol
case class Or(left: ExprSymbol, right: ExprSymbol) extends ExprSymbol
case class And(left: ExprSymbol, right: ExprSymbol) extends ExprSymbol
case class Not(expr: ExprSymbol) extends ExprSymbol

/**
 * Exceptions
 */
final case class SimpleKeywordRuleParseException(private val message: String = "",
                                                 private val cause: Throwable = None.orNull)
  extends Exception(message, cause)