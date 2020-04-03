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

//  def tokenize(s: String): Stream[Token] =
//    "[0-9]+|[-+/^*()]".r.findAllMatchIn(s).map(_.matched).map { n =>
//      if (n.head.isDigit)
//        ValueToken(n.toInt)
//      else
//        symbols get n match {
//          case None            => sys.error(s"unrecognized token: $n")
//          case Some(null)      => DelimiterToken(n)
//          case Some(operators) => OperatorToken(n, operators)
//        }
//    } ++ Iterator(EOI) toStream

  val symbols =
    Map[String, (PrefixOperator, InfixOperator)](
      "+" -> (PrefixOperator(30, +_), InfixOperator(10, LeftAssoc, _ + _)),
      "-" -> (PrefixOperator(30, -_), InfixOperator(10, LeftAssoc, _ - _)),
      "*" -> (null, InfixOperator(20, LeftAssoc, _ * _)),
      "/" -> (null, InfixOperator(20, LeftAssoc, _ / _)),
      "^" -> (null, InfixOperator(30, RightAssoc, pow)),
      "(" -> null,
      ")" -> null
    )

  def pow(x: Int, n: Int) = {
    def f(x: Int, n: Int, y: Int): Int = {
      def g(x: Int, n: Int): Int =
        if (n % 2 == 0)
          g(x * x, n / 2)
        else
          f(x, n - 1, x * y)

      if (n == 0) y else g(x, n)
    }

    n match {
      case 0 if x == 0 => sys.error(s"pow: zero to the power of zero is undefined")
      case 0           => 1
      case _ if n > 0  => f(x, n - 1, x)
      case _           => sys.error(s"pow: negative exponent: $n")
    }
  }

  def parse(toks: Stream[Token]) = {
    var cur = toks

    def next = cur = cur.tail

    def token = cur.head

    def consume = {
      val res = token

      next
      res
    }

    def infixOperator = token.asInstanceOf[OperatorToken].operators._2

    def isInfix = token.isInstanceOf[OperatorToken] && infixOperator != null

    def parse(minPrec: Int): Int = {
      var result = primitive

      while (isInfix && infixOperator.prec >= minPrec) {
        val InfixOperator(prec, assoc, compute) = infixOperator
        val nextMinPrec                         = if (assoc == LeftAssoc) prec + 1 else prec

        next
        result = compute(result, parse(nextMinPrec))
      }

      result
    }

    def primitive =
      consume match {
        case DelimiterToken(_, _, "(") =>
          val result = parse(0)

          expect(")", "expected closing parenthesis")
          result
        case ValueToken(_, _, value)                               => value
        case OperatorToken(_, _, _, (prefix, _)) if prefix ne null => prefix.compute(parse(prefix.prec))
        case OperatorToken(_, _, _, (_, infix)) if infix ne null =>
          sys.error(s"expected a primitive expression, not an infix operator: $token")
      }

    def expect(name: String, error: String) = {
      if (token.name != name)
        sys.error(s"$error: $token")

      next
    }

    val result = parse(0)

    expect("EOI", "expected end of input")
    result
  }

  abstract class Token { val line: Int; val col: Int; val name: String }
  case class DelimiterToken(line: Int, col: Int, name: String)                                            extends Token
  case class ValueToken(line: Int, col: Int, value: Int)                                                  extends Token { val name = "int" }
  case class OperatorToken(line: Int, col: Int, name: String, operators: (PrefixOperator, InfixOperator)) extends Token
  case class EOI(line: Int, col: Int)                                                                     extends Token { val name = "EOI" }

  abstract class Assoc
  case object LeftAssoc  extends Assoc
  case object RightAssoc extends Assoc

  abstract class Operator
  case class InfixOperator(prec: Int, assoc: Assoc, compute: (Int, Int) => Int) extends Operator
  case class PrefixOperator(prec: Int, compute: Int => Int)                     extends Operator

}
