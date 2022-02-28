package gavin.parser

import org.scalatest._
import flatspec._
import matchers._

class SimpleKeywordRuleParserEBNFSuite extends AnyFlatSpec with should.Matchers {
  val ruleParser = new SimpleKeywordRuleParserEBNF

  "AST from Simple keyword rule parser" should "shaped in tree what expected" in {
    val testRuleAST = ruleParser.parseAST("""(청바지 and !면바지) or "반바지"""")
    val expectedAST = Or(And(Keyword("청바지"), Not(Keyword("면바지"))), Keyword("반바지"))
    testRuleAST should be (expectedAST)
  }

  it should "And operator's priority is higher than Or" in {
    val testRuleAST = ruleParser.parseAST("""청바지 and 면바지 or "반바지"""")
    val expectedAST = Or(And(Keyword("청바지"), Keyword("면바지")), Keyword("반바지"))
    testRuleAST should be (expectedAST)
  }

  it should "Not operator's priority is higher than And/Or" in {
    val testRuleAST = ruleParser.parseAST("""!청바지 and 면바지 or !"반바지"""")
    val expectedAST = Or(And(Not(Keyword("청바지")), Keyword("면바지")), Not(Keyword("반바지")))
    testRuleAST should be (expectedAST)
  }
}
