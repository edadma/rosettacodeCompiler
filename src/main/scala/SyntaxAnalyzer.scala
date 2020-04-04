package xyz.hyperreal.rosettacodeCompiler

import scala.io.Source

object SyntaxAnalyzer extends App {

  fromString("""
               |    4      1 Keyword_print
               |    4      6 LeftParen
               |    4      7 Integer         345
               |    4     24 RightParen
               |    4     25 Semicolon
               |    5      1 End_of_input
               |""".trim.stripMargin)

  lazy val symbols =
    Map[String, (PrefixOperator, InfixOperator)](
      "+" -> (PrefixOperator(30, +_.asInstanceOf[Int]), InfixOperator(10,
                                                                      LeftAssoc,
                                                                      _.asInstanceOf[Int] + _.asInstanceOf[Int])),
      "-" -> (PrefixOperator(30, -_.asInstanceOf[Int]), InfixOperator(10,
                                                                      LeftAssoc,
                                                                      _.asInstanceOf[Int] - _.asInstanceOf[Int])),
      "*" -> (null, InfixOperator(20, LeftAssoc, _.asInstanceOf[Int] * _.asInstanceOf[Int])),
      "/" -> (null, InfixOperator(20, LeftAssoc, _.asInstanceOf[Int] / _.asInstanceOf[Int])),
      "^" -> (null, InfixOperator(30, RightAssoc, (a, b) => pow(a.asInstanceOf[Int], b.asInstanceOf[Int]))),
      "(" -> null,
      ")" -> null
    )

  private def pow(x: Int, n: Int) = {
    def pow(y: Int, x: Int, n: Int): Int =
      n match {
        case 0 => 1
        case 1 => x * y
        case _ if n % 2 == 0 => pow(y, x * x, n / 2)
        case _ => pow(x * y, x * x, (n - 1) / 2)
      }

    if (n < 0) sys.error(s"pow: negative exponent: $n") else pow(1, x, n)
  }

  def fromStdin = fromSource(Source.stdin)

  def fromString(src: String) = fromSource(Source.fromString(src))

  def fromSource(s: Source) = {
    val tokens = (s.getLines map (_.trim.split(" +", 4)) map {
      case Array(line, col, name) =>
        symbols get name match {
          case None | Some(null) => SimpleToken(line.toInt, col.toInt, name)
          case Some(operators)   => OperatorToken(line.toInt, col.toInt, name, operators)
        }
      case Array(line, col, name, value) => ValueToken(line.toInt, col.toInt, name, value)
    }) ++ Iterator(EOI) toStream

    println(tokens mkString "\n")
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

    def accept(name: String) =
      if (token.name == name) {
        next
        true
      } else
        false

    def expect(name: String, error: String = null) =
      if (consume.name != name)
        sys.error(if (error eq null) s"expected $name" else s"$error: $token")

    def expression(minPrec: Int): Any = {
      def infixOperator = token.asInstanceOf[OperatorToken].operators._2

      def isInfix = token.isInstanceOf[OperatorToken] && infixOperator != null

      var result =
        consume match {
          case SimpleToken(_, _, "(") =>
            val result = expression(0)

            expect(")", "expected closing parenthesis")
            result
          case ValueToken(_, _, "Integer", value)                    => value.toInt
          case OperatorToken(_, _, _, (prefix, _)) if prefix ne null => prefix.compute(expression(prefix.prec))
          case OperatorToken(_, _, _, (_, infix)) if infix ne null =>
            sys.error(s"expected a primitive expression, not an infix operator: $token")
        }

      while (isInfix && infixOperator.prec >= minPrec) {
        val InfixOperator(prec, assoc, compute) = infixOperator
        val nextMinPrec                         = if (assoc == LeftAssoc) prec + 1 else prec

        next
        result = compute(result, expression(nextMinPrec))
      }

      result
    }

    if (accept("Keyword_print")) {
      expect("(")

      val arg = expression(0)

      expect(")")
      expect(";")
      println("Prti")
    } else
      sys.error(s"syntax error: $token")

    expect("EOI", "expected end of input")
  }

  abstract class Token {
    val line: Int;
    val col: Int;
    val name: String
  }

  case class SimpleToken(line: Int, col: Int, name: String)                                               extends Token
  case class ValueToken(line: Int, col: Int, name: String, value: String)                                 extends Token
  case class OperatorToken(line: Int, col: Int, name: String, operators: (PrefixOperator, InfixOperator)) extends Token
  case class EOI(line: Int, col: Int) extends Token {
    val name = "EOI"
  }

  abstract class Assoc
  case object LeftAssoc  extends Assoc
  case object RightAssoc extends Assoc

  abstract class Operator
  case class InfixOperator(prec: Int, assoc: Assoc, compute: (Any, Any) => Any) extends Operator
  case class PrefixOperator(prec: Int, compute: Any => Any)                     extends Operator

}
