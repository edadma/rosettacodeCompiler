package xyz.hyperreal.rosettacodeCompiler

import scala.io.Source

object SyntaxAnalyzer extends App {

  fromString("""
               |    4      1 Keyword_print
               |    4      6 LeftParen
               |    4      7 String         "Hello, World!\n"
               |    4     24 RightParen
               |    4     25 Semicolon
               |    5      1 End_of_input
               |""".trim.stripMargin)

  lazy val symbols =
    Map[String, (PrefixOperator, InfixOperator)](
      "+" -> (PrefixOperator(30, identity), InfixOperator(10, LeftAssoc, BranchNode("Add", _, _))),
      "-" -> (PrefixOperator(30, BranchNode("Negate", _, TerminalNode)), InfixOperator(10,
                                                                                       LeftAssoc,
                                                                                       BranchNode("Subtract", _, _))),
      "*" -> (null, InfixOperator(20, LeftAssoc, BranchNode("Multiply", _, _))),
      "/" -> (null, InfixOperator(20, LeftAssoc, BranchNode("Divide", _, _))),
      "%" -> (null, InfixOperator(30, RightAssoc, BranchNode("Mod", _, _))),
      "(" -> null,
      ")" -> null
    )

  def fromStdin = fromSource(Source.stdin)

  def fromString(src: String) = fromSource(Source.fromString(src))

  def fromSource(s: Source) = {
    val tokens = ((s.getLines map (_.trim.split(" +", 4)) map {
      case Array(line, col, name) =>
        symbols get name match {
          case None | Some(null) => SimpleToken(line.toInt, col.toInt, name)
          case Some(operators)   => OperatorToken(line.toInt, col.toInt, name, operators)
        }
      case Array(line, col, name, value) => ValueToken(line.toInt, col.toInt, name, value)
    }) toStream)

    println(tokens mkString "\n")
    println(parse(tokens))
    flatten(parse(tokens))
  }

  def flatten(n: Node): Unit =
    n match {
      case TerminalNode          => println(";")
      case LeafNode(name, value) => println(s"$name $value")
      case BranchNode(name, left, right) =>
        println(name)
        flatten(left)
        flatten(right)
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
      if (token.name != name)
        sys.error(if (error eq null) s"expected $name, found ${token.name}" else s"$error: $token")
      else
        next

    def expression(minPrec: Int): Node = {
      def infixOperator = token.asInstanceOf[OperatorToken].operators._2

      def isInfix = token.isInstanceOf[OperatorToken] && infixOperator != null

      var result =
        consume match {
          case SimpleToken(_, _, "(") =>
            val result = expression(0)

            expect(")", "expected closing parenthesis")
            result
          case ValueToken(_, _, name, value)                         => LeafNode(name, value)
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

    var tree: Node = TerminalNode

    if (accept("Keyword_print")) {
      expect("LeftParen")

      if (token.name == "String")
        tree = BranchNode("Prts", LeafNode("String", consume.asInstanceOf[ValueToken].value), TerminalNode)
      else
        tree = BranchNode("Prti", expression(0), TerminalNode)

      expect("RightParen")
      expect("Semicolon")
    } else
      sys.error(s"syntax error: $token")

    expect("End_of_input")
    tree
  }

  abstract class Node
  case class LeafNode(name: String, value: String)             extends Node
  case class BranchNode(name: String, left: Node, right: Node) extends Node
  case object TerminalNode                                     extends Node

  abstract class Token {
    val line: Int;
    val col: Int;
    val name: String
  }

  case class SimpleToken(line: Int, col: Int, name: String)                                               extends Token
  case class ValueToken(line: Int, col: Int, name: String, value: String)                                 extends Token
  case class OperatorToken(line: Int, col: Int, name: String, operators: (PrefixOperator, InfixOperator)) extends Token

  abstract class Assoc
  case object LeftAssoc  extends Assoc
  case object RightAssoc extends Assoc

  abstract class Operator
  case class InfixOperator(prec: Int, assoc: Assoc, compute: (Node, Node) => Node) extends Operator
  case class PrefixOperator(prec: Int, compute: Node => Node)                      extends Operator

}
