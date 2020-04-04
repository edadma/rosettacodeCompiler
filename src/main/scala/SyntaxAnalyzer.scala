package xyz.hyperreal.rosettacodeCompiler

import scala.io.Source

object SyntaxAnalyzer {

  val symbols =
    Map[String, (PrefixOperator, InfixOperator)](
      "Op_add" -> (PrefixOperator(30, identity), InfixOperator(10, LeftAssoc, BranchNode("Add", _, _))),
      "Op_minus" -> (PrefixOperator(30, BranchNode("Negate", _, TerminalNode)), InfixOperator(
        10,
        LeftAssoc,
        BranchNode("Subtract", _, _))),
      "Op_multiply" -> (null, InfixOperator(20, LeftAssoc, BranchNode("Multiply", _, _))),
      "Op_divide"   -> (null, InfixOperator(20, LeftAssoc, BranchNode("Divide", _, _))),
      "Op_mod"      -> (null, InfixOperator(30, RightAssoc, BranchNode("Mod", _, _))),
      "LeftParen"   -> null,
      "RightParen"  -> null
    )

  def apply = new SyntaxAnalyzer(symbols)

  abstract class Node
  case class LeafNode(name: String, value: String)             extends Node
  case class BranchNode(name: String, left: Node, right: Node = TerminalNode) extends Node
  case object TerminalNode                                     extends Node

  abstract class Assoc
  case object LeftAssoc  extends Assoc
  case object RightAssoc extends Assoc

  abstract class Operator
  case class InfixOperator(prec: Int, assoc: Assoc, compute: (Node, Node) => Node) extends Operator
  case class PrefixOperator(prec: Int, compute: Node => Node)                      extends Operator

}

class SyntaxAnalyzer(symbols: Map[String, (SyntaxAnalyzer.PrefixOperator, SyntaxAnalyzer.InfixOperator)]) {
  import SyntaxAnalyzer.{BranchNode, InfixOperator, LeafNode, LeftAssoc, Node, PrefixOperator, TerminalNode}

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

    //println(tokens mkString "\n")
    //println(parse(tokens))
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

    var tree: Node = null

    def parenExpression = {
      expect("LeftParen")

      val e = expression(0)

      expect("RightParen")
      e
    }

    def statement = {
      tree = TerminalNode

      if (accept("Keyword_putc")) {
        tree = BranchNode("Prtc", parenExpression)
        expect("Semicolon")
      } else     if (accept("Keyword_print")) {
          expect("LeftParen")

          do {
            val e =
              if (token.name == "String")
                BranchNode("Prts", LeafNode("String", consume.asInstanceOf[ValueToken].value), TerminalNode)
              else
                BranchNode("Prti", expression(0), TerminalNode)

            tree = BranchNode("Sequence", tree, e)
          } while (accept("Comma"))

          expect("RightParen")
          expect("Semicolon")
        } else
          sys.error(s"syntax error: $token")
      }

      tree
    }

    tree = TerminalNode

    do {
      tree = BranchNode("Sequence", tree, statement)
    } while (token.name != "End_of_input")

    expect("End_of_input")
    tree
  }

  abstract class Token {
    val line: Int;
    val col: Int;
    val name: String
  }

  case class SimpleToken(line: Int, col: Int, name: String)                                               extends Token
  case class ValueToken(line: Int, col: Int, name: String, value: String)                                 extends Token
  case class OperatorToken(line: Int, col: Int, name: String, operators: (PrefixOperator, InfixOperator)) extends Token

}
