package xyz.hyperreal.rosettacodeCompiler

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.io.Source
import scala.util.matching.Regex

object LexicalAnalyzer {
  private val SYMBOL = ('!' to '/' toSet) ++ (':' to '@') ++ ('[' to '`') ++ ('{' to '~')
  private val EOT    = '\u0004'
}

class LexicalAnalyzer(tabs: Int,
                      symbols: Map[String, String],
                      delimiters: Map[Char, String],
                      keywords: Map[String, String],
                      endOfInput: String,
                      identifier: Token,
                      tokens: Token*) {

  import LexicalAnalyzer._

  private var curline: Int = _
  private var curcol: Int  = _

  def fromStdin = fromSource(Source.stdin)

  def fromString(src: String) = fromSource(Source.fromString(src))

  def fromSource(ast: Source) = {
    var s: Stream[Chr] = (ast map (new Chr(_)) toStream) :+ new Chr(EOT)

    curline = 1
    curcol = 1
    tokenize

    def token(name: String) = println(f"$curline%5d $curcol%6d $name%-14s")

    def until(c: Char) = {
      val buf = new StringBuilder

      def until: String =
        if (s.head.c == EOT || s.head.c == c)
          buf.toString
        else {
          buf += s.head.c
          s = s.tail
          until
        }

      until
    }

    def consume(cs: Set[Char]) = {
      val buf = new StringBuilder

      def consume: String =
        if (s.head.c == EOT || !cs(s.head.c))
          buf.toString
        else {
          buf += s.head.c
          s = s.tail
          consume
        }

      consume
    }

    def comment: Unit = {
      until('*')

      if (s.head.c == EOT || s.tail.head.c == EOT)
        sys.error(s"unclosed comment ${s.tail.head.at}")
      else if (s.tail.head.c != '/') {
        s = s.tail
        comment
      } else
        s = s.tail.tail
    }

    def recognize(t: Token) =
      t match {
        case StartRestToken(name, start, rest) =>
          if (start(s.head.c)) {
            val first = s.head.c

            s = s.tail

            val m = first +: consume(rest)
          }
        case SimpleToken(name, pattern, exclude, excludeError)                     =>
        case DelimitedToken(name, delimiter, pattern, patternError, unclosedError) =>
      }

    def tokenize: Unit =
      if (s.head.c == EOT)
        token(endOfInput)
      else {
        if (s.head.c.isWhitespace)
          s = s.tail
        else if (s.head.c == '/' && s.head.c == '*')
          comment
        else
          delimiters get s.head.c match {
            case Some(name) =>
              s = s.tail
              token(name)
            case None =>
              if (SYMBOL(s.head.c)) {
                val buf = new StringBuilder

                while (s.head.c != EOT && !delimiters.contains(s.head.c) && SYMBOL(s.head.c)) {
                  buf += s.head.c
                  s = s.tail
                }

                symbols get buf.toString match {
                  case Some(name) => token(name)
                  case None       => sys.error(s"unrecognized symbol: '${buf.toString}' ${s.head.at}")
                }
              } else {}
          }

        tokenize
      }
  }

  private class Chr(val c: Char) {
    val line = curline
    val col  = curcol

    if (c == '\n') {
      curline += 1
      curcol = 1
    } else if (c == '\r')
      curcol = 1
    else if (c == '\t')
      curcol += tabs - (curcol - 1) % tabs
    else
      curcol += 1

    def at = s"[${line}, ${col}]"
  }

}

abstract class Token
case class StartRestToken(name: String, start: Set[Char], rest: Set[Char])                         extends Token
case class SimpleToken(name: String, pattern: Set[Char], exclude: Set[Char], excludeError: String) extends Token
case class DelimitedToken(name: String, delimiter: Char, pattern: Regex, patternError: String, unclosedError: String)
    extends Token
