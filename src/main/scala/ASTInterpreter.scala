package xyz.hyperreal.rosettacodeCompiler

import scala.collection.mutable
import scala.io.Source

object ASTInterpreter {

  def fromStdin = fromSource(Source.stdin)

  def fromString(src: String) = fromSource(Source.fromString(src))

  def fromSource(s: Source) = {
    val lines = s.getLines

    def load: Node =
      if (!lines.hasNext)
        TerminalNode
      else {
        val node = lines.next.split(" +", 2)

        node match {
          case Array(name, value) => LeafNode(name, value)
          case Array(";")         => TerminalNode
          case Array(name)        => BranchNode(name, load, load)
        }
      }

    val vars = new mutable.HashMap[String, Any]

    def interpInt(n: Node) = interp(n).asInstanceOf[Int]

    def interpBoolean(n: Node) = interp(n).asInstanceOf[Boolean]

    def interp(n: Node): Any =
      n match {
        case TerminalNode => null
        case LeafNode("Identifier", name) =>
          vars get name match {
            case None =>
              vars(name) = 0
              0
            case Some(v) => v
          }
        case LeafNode("Integer", "'\\n'")                               => '\n'.toInt
        case LeafNode("Integer", "'\\\\'")                              => '\\'.toInt
        case LeafNode("Integer", value: String) if value startsWith "'" => value(1).toInt
        case LeafNode("Integer", value: String)                         => value.toInt
        case LeafNode("String", value: String) =>
          value.substring(1, value.length - 1).replace("\\n", "\n").replace("\\\\", "\\")
        case BranchNode("Assign", LeafNode(_, name), exp) => vars(name) = interp(exp)
        case BranchNode("Sequence", l, r)                 => interp(l); interp(r)
        case BranchNode("Prts" | "Prti", a, _)            => print(interp(a))
        case BranchNode("Prtc", a, _)                     => print(interpInt(a).toChar)
        case BranchNode("Add", l, r)                      => interpInt(l) + interpInt(r)
        case BranchNode("Less", l, r)                     => interpInt(l) < interpInt(r)
        case BranchNode("While", l, r)                    => while (interpBoolean(l)) interp(r)
      }

    interp(load)

  }

  abstract class Node
  case class BranchNode(name: String, left: Node, right: Node) extends Node
  case class LeafNode(name: String, value: String)             extends Node
  case object TerminalNode                                     extends Node
}
