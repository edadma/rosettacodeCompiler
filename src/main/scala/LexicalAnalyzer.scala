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
    curline = 1
    curcol = 1

    var s = (ast ++ Iterator(EOT)) map (new Chr(_)) toStream

    tokenize

    def token(name: String, first: Chr) = println(f"${first.line}%5d ${first.col}%6d $name%-14s")

    def value(name: String, v: String, first: Chr) = println(f"${first.line}%5d ${first.col}%6d $name%-14s $v")

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

    def consume(first: Char, cs: Set[Char]) = {
      val buf = new StringBuilder

      def consume: String =
        if (s.head.c == EOT || !cs(s.head.c))
          buf.toString
        else {
          buf += s.head.c
          s = s.tail
          consume
        }

      buf += first
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

    def recognize(t: Token): Option[(String, String)] = {
      val first = s.head.c

      s = s.tail

      t match {
        case StartRestToken(name, start, rest) =>
          if (start(first)) {
            Some((name, consume(first, rest)))
          } else
            None
        case SimpleToken(name, chars, exclude, excludeError) =>
          if (chars(first))
            None
          else {
            val m = consume(first, chars)

            if (exclude(s.head.c))
              sys.error(s"$excludeError ${s.head.at}")
            else
              Some((name, m))
          }
        case DelimitedToken(name, delimiter, pattern, patternError, unclosedError) =>
          if (first == delimiter) {
            val m = until(delimiter)

            if (s.head.c != delimiter)
              sys.error(s"$unclosedError ${s.head.at}")
            else if (pattern.pattern.matcher(m).matches) {
              s = s.tail
              Some((name, m))
            } else
              sys.error(s"$patternError ${s.head.at}")
          } else
            None
      }
    }

    def tokenize: Unit =
      if (s.head.c == EOT)
        token(endOfInput, s.head)
      else {
        if (s.head.c.isWhitespace)
          s = s.tail
        else if (s.head.c == '/' && s.head.c == '*')
          comment
        else
          delimiters get s.head.c match {
            case Some(name) =>
              token(name, s.head)
              s = s.tail
            case None =>
              if (SYMBOL(s.head.c)) {
                val first = s.head
                val buf   = new StringBuilder

                while (s.head.c != EOT && !delimiters.contains(s.head.c) && SYMBOL(s.head.c)) {
                  buf += s.head.c
                  s = s.tail
                }

                symbols get buf.toString match {
                  case Some(name) => token(name, first)
                  case None       => sys.error(s"unrecognized symbol: '${buf.toString}' ${s.head.at}")
                }
              } else {
                val first = s.head

                recognize(identifier) match {
                  case None                =>
                  case Some((name, ident)) => value(name, ident, first)
                }
              }
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

    override def toString: String = s"<$c, $line, $col>"
  }

}

abstract class Token
case class StartRestToken(name: String, start: Set[Char], rest: Set[Char])                       extends Token
case class SimpleToken(name: String, chars: Set[Char], exclude: Set[Char], excludeError: String) extends Token
case class DelimitedToken(name: String, delimiter: Char, pattern: Regex, patternError: String, unclosedError: String)
    extends Token
