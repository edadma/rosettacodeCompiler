package xyz.hyperreal.rosettacodeCompiler

object Testing {

  def run(src: String) =
    capture(
      ASTInterpreter.fromString(
        capture(SyntaxAnalyzer.apply.fromString(capture(LexicalAnalyzer.apply.fromString(src))))))

}
