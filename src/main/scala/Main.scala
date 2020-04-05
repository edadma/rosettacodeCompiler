package xyz.hyperreal.rosettacodeCompiler

import java.nio.file.Path

object Main extends App {

  case class Options(
      lexer: Boolean = false,
      parser: Boolean = false,
      vm: Boolean = false,
      gen: Boolean = false,
      source: Boolean = false,
      input: Option[Path] = None,
      output: Option[Path] = None
  )

  private val optionsParser = new scopt.OptionParser[Options]("rosettacodeCompiler") {
    head("Rosetta Code Compiler", "v0.1")
    help("help").text("print this usage text").abbr("h")
    opt[Unit]('g', "gen")
      .action((_, c) => c.copy(gen = true))
      .text("run code generator only")
    opt[Unit]('l', "lexer")
      .action((_, c) => c.copy(lexer = true))
      .text("run lexer only")
    opt[Unit]('p', "parser")
      .action((_, c) => c.copy(parser = true))
      .text("run parser (only unless -s is used)")
    opt[Unit]('s', "source")
      .action((_, c) => c.copy(source = true))
      .text("run all prior steps")
    opt[Unit]('v', "vm")
      .action((_, c) => c.copy(vm = true))
      .text("run using virtual machine (only unless -s is used)")
    version("version").text("print the version").abbr("v")
  }

  optionsParser.parse(args, Options()) match {
    case Some(options) =>
      if (options.lexer)
        LexicalAnalyzer.apply.fromStdin
      else if (options.parser)
        SyntaxAnalyzer.apply.fromStdin
      else if (options.gen && options.source)
        CodeGenerator.fromString(capture(SyntaxAnalyzer.apply.fromString(capture(LexicalAnalyzer.apply.fromStdin))))
      else if (options.gen)
        CodeGenerator.fromStdin
      else if (options.vm && options.source)
        VirtualMachine
          .fromString(
            capture(CodeGenerator.fromString(
              capture(SyntaxAnalyzer.apply.fromString(capture(LexicalAnalyzer.apply.fromStdin))))))
          .run
      else if (options.vm)
        VirtualMachine.fromStdin.run
      else
        ASTInterpreter.fromString(capture(SyntaxAnalyzer.apply.fromString(capture(LexicalAnalyzer.apply.fromStdin))))
    case None => sys.exit(1)
  }

}
