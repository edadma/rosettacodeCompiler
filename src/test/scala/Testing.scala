package xyz.hyperreal.rosettacodeCompiler

object Testing {

  def runUsingInterpreter(src: String) =
    capture(
      ASTInterpreter.fromString(
        capture(SyntaxAnalyzer.apply.fromString(capture(LexicalAnalyzer.apply.fromString(src))))))

  def runUsingVM(src: String) =
    capture(
      VirtualMachine
        .fromString(capture(CodeGenerator.fromString(
          capture(SyntaxAnalyzer.apply.fromString(capture(LexicalAnalyzer.apply.fromString(src)))))))
        .run)

}
