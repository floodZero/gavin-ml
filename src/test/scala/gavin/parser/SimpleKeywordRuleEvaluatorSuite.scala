package gavin.parser

import org.scalatest._
import flatspec._
import matchers._

class SimpleKeywordRuleEvaluatorSuite extends AnyFlatSpec with should.Matchers {
  val ruleParser = new SimpleKeywordRuleParserEBNF
  val testRuleAST: ExprSymbol = ruleParser.parseAST("""(청바지 and !면바지) or "반바지"""")

  val evaluator = new SimpleKeywordRuleEvaluator

  "Simple keyword rule AST evaluation" should "return expected true/false" in {
    val testPairs = List(
      ("청바지", true),
      ("반바지", true),
      ("청바지 면바지", false),
      ("반바지 청바지", true),
      ("면바지 반바지 청바지", true),
      ("면바지 청바지", false)
    )

    testPairs.foreach { case (targetStr, expectedReturn) =>
      evaluator.eval(testRuleAST, targetStr, null) should be(expectedReturn)
    }
  }
}
