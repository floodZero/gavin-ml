package gavin.parser

class SimpleKeywordRuleEvaluator extends Serializable {
  /**
   * Evaluator uses custom function to evaluate Keyword symbol.
   * Evaluated by depth first search(DFS).
   * Keyword is scan and evaluated every traversal.
   *
   * @param ast                Root of Abstract Syntax Tree (AST) of rule to evaluate
   * @param targetStr          String to evaluate using the AST
   * @param keywordEvalFunc    Function to evaluate "Keyword" symbol
   * @return
   */
  def eval(ast: ExprSymbol, targetStr: String, keywordEvalFunc: String => Boolean): Boolean = {
    val defaultKeywordEvalFunc = {keyword: String => targetStr.contains(keyword)}

    if (keywordEvalFunc == null) {
      exprSymbolTraversal(ast, defaultKeywordEvalFunc)
    } else {
      exprSymbolTraversal(ast, keywordEvalFunc)
    }
  }

  def exprSymbolTraversal(partialAST: ExprSymbol, keywordEvalFunc: String => Boolean): Boolean = {
    partialAST match {
      case node: Or =>
        val leftRet = exprSymbolTraversal(node.left, keywordEvalFunc)
        // Shortcut for "Or" operation
        if (leftRet)
          return true

        exprSymbolTraversal(node.right, keywordEvalFunc)

      case node: And =>
        val leftRet = exprSymbolTraversal(node.left, keywordEvalFunc)
        // Shortcut for "And" operation
        if (!leftRet)
          return false

        exprSymbolTraversal(node.right, keywordEvalFunc)

      case node: Not =>
        !exprSymbolTraversal(node.expr, keywordEvalFunc)

      case node: Keyword =>
        keywordEvalFunc.apply(node.term)

      case notImplemented =>
        throw SimpleKeywordRuleEvaluationException(s"Not implemented AST node. Class name: ${notImplemented.getClass.getName}")
    }
  }
}

/**
 * Exceptions
 */
final case class SimpleKeywordRuleEvaluationException(private val message: String = "",
                                                      private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

