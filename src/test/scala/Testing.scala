package xyz.hyperreal.rosettacodeCompiler

object Testing {

  def runUsingInterpreter(src: String) =
    capture(
      ASTInterpreter.fromString(
        capture(SyntaxAnalyzer.apply.fromString(capture(LexicalAnalyzer.apply.fromString(src))))))

}
