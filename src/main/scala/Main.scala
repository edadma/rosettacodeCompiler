package xyz.hyperreal.rosettacodeCompiler

import java.nio.file.Path

object Main extends App {

  case class Options(
      lexer: Boolean = false,
      parser: Boolean = false,
      run: Boolean = false,
      gen: Boolean = false,
      source: Boolean = false,
      input: Option[Path] = None,
      output: Option[Path] = None
  )

  private val optionsParser = new scopt.OptionParser[Options]("rosettacodeCompiler") {
    head("Rosetta Code Compiler", "v0.1")
    opt[Unit]('g', "gen")
      .action((_, c) => c.copy(gen = true))
      .text("run code generator (only unless -s is used)")
    help("help").text("print this usage text").abbr("h")
    opt[Unit]('i', "interp")
      .action((_, c) => c.copy(gen = true))
      .text("run interpreter (only unless -s is used)")
    opt[Unit]('l', "lexer")
      .action((_, c) => c.copy(lexer = true))
      .text("run lexer only")
    opt[Unit]('p', "parser")
      .action((_, c) => c.copy(parser = true))
      .text("run parser (only unless -s is used)")
    opt[Unit]('r', "run")
      .action((_, c) => c.copy(run = true))
      .text("run using virtual machine (only unless -s is used)")
    opt[Unit]('s', "source")
      .action((_, c) => c.copy(source = true))
      .text("run all prior steps")
    version("version").text("print the version").abbr("v")
  }

  optionsParser.parse(args, Options()) match {
    case Some(options) =>
      if (options.lexer)
        LexicalAnalyzer.apply.fromStdin
      else if (options.parser && options.source)
        SyntaxAnalyzer.apply.fromString(capture(LexicalAnalyzer.apply.fromStdin))
      else if (options.parser)
        SyntaxAnalyzer.apply.fromStdin
      else if (options.gen && options.source)
        CodeGenerator.fromString(capture(SyntaxAnalyzer.apply.fromString(capture(LexicalAnalyzer.apply.fromStdin))))
      else if (options.gen)
        CodeGenerator.fromStdin
      else if (options.run && options.source)
        VirtualMachine
          .fromString(
            capture(CodeGenerator.fromString(
              capture(SyntaxAnalyzer.apply.fromString(capture(LexicalAnalyzer.apply.fromStdin))))))
          .run
      else if (options.run)
        VirtualMachine.fromStdin.run
      else if (options.source) {
        optionsParser.showUsageAsError
        optionsParser.reportError("-s should be used with another option")
      } else
        ASTInterpreter.fromString(capture(SyntaxAnalyzer.apply.fromString(capture(LexicalAnalyzer.apply.fromStdin))))
    case None => sys.exit(1)
  }

}
