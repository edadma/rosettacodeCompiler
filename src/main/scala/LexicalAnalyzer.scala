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

    def until(c: Char): (String, Stream[Chr]) = {
      val buf = new StringBuilder

      def until: (String, Stream[Chr]) =
        if (s.head.c == EOT || s.head.c == c)
          (buf.toString, s)
        else {
          buf += s.head.c
          s = s.tail
          until
        }

      until
    }

    def comment: Unit = {
      val (_, r) = until('*')

      if (r.head.c == EOT || r.tail.head.c == EOT)
        sys.error(s"unclosed comment ${r.tail.head.at}")
      else if (r.tail.head.c != '/') {
        s = r.tail
        comment
      } else
        s = r.tail.tail
    }

    def tokenize: Unit = {
      val chr = s.head

      s = s.tail

      if (chr.c == EOT)
        token(endOfInput)
      else {
        if (!chr.c.isWhitespace)
          if (chr.c == '/' && s.head.c == '*')
            comment
          else
            delimiters get chr.c match {
              case Some(name) => token(name)
              case None =>
                if (SYMBOL(chr.c)) {
                  val buf = new StringBuilder

                  while (s.head.c != EOT && !delimiters.contains(s.head.c) && SYMBOL(s.head.c)) {
                    buf += s.head.c
                    s = s.tail
                  }

                  symbols get buf.toString match {
                    case Some(name) => token(name)
                    case None       => sys.error(s"unrecognized symbol: '${buf.toString}' ${chr.at}")
                  }
                }
            }

        tokenize
      }
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
case class StartRestToken(name: String, start: Regex, rest: Regex)                         extends Token
case class SimpleToken(name: String, pattern: Regex, exclude: Regex, excludeError: String) extends Token
case class DelimitedToken(name: String, delimiter: Char, pattern: Regex, patternError: String, unclosedError: String)
    extends Token
