package xyz.hyperreal.rosettacodeCompiler

import scala.io.Source

object SyntaxAnalyzer {

  def fromStdin = fromSource(Source.stdin)

  def fromString(src: String) = fromSource(Source.fromString(src))

  def fromSource(s: Source) = {
    val tokens = s.getLines map (_.trim.split(" +", 4)) map {
      case Array(line, col, name)        => Token(line.toInt, col.toInt, name, null)
      case Array(line, col, name, value) => Token(line.toInt, col.toInt, name, value)
    }

    println(tokens.toList mkString "\n")
  }

  case class Token(line: Int, col: Int, name: String, value: String)

}
