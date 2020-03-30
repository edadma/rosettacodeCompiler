package xyz.hyperreal.rosettacodeCompiler

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}
import scala.io.Source

object CodeGenerator {

  def fromStdin = fromSource(Source.stdin)

  def fromString(src: String) = fromSource(Source.fromString(src))

  def fromSource(ast: Source) = {
    val vars    = new LinkedHashMap[String, Int]
    val strings = new ArrayBuffer[String]
    val code    = new ArrayBuffer[Inst]
    val it      = ast.getLines

    def line =
      if (it.hasNext) {
        it.next.split(" +", 2) match {
          case Array(n) => n
          case a        => a
        }
      } else
        sys.error("unexpected end of AST")

    def variable(name: String) =
      vars get name match {
        case None =>
          val idx = vars.size

          vars(name) = idx
          idx
        case Some(idx) => idx
      }

    var loc = 0

    generate
    code += HaltInst
    println(s"Datasize: ${vars.size} Strings: ${strings.length}")

    for (s <- strings)
      println(s""""$s"""")

    for (inst <- code) {
      print(f"$loc%4d ")
      println(inst match {
        case PushInst(n)    => s"push  $n"
        case PrtsInst       => "prts"
        case PrtiInst       => "prti"
        case PrtcInst       => "prtc"
        case HaltInst       => "halt"
        case StoreInst(idx) => s"store [$idx]"
        case FetchInst(idx) => s"fetch [$idx]"
      })

      loc += (if (inst.isInstanceOf[OperandInst]) 5 else 1)
    }

    def generate: Unit =
      line match {
        case "Sequence" =>
          generate
          generate
        case ";" =>
        case "Assign" =>
          val idx =
            line match {
              case Array("Identifier", name: String) => variable(name)
              case l                                 => sys.error(s"expected identifier: $l")
            }

          generate
          code += StoreInst(idx)
        case Array("Identifier", name: String) => code += FetchInst(variable(name))
        case Array("Integer", n: String)       => code += PushInst(n.toInt)
        case "Prti" =>
          generate
          code += PrtiInst
      }
  }

}

abstract class Inst
abstract class JumpInst extends OperandInst { var disp: Int }
trait OperandInst
case class PushInst(n: Int)       extends Inst with OperandInst
case object PrtsInst              extends Inst
case object PrtiInst              extends Inst
case object PrtcInst              extends Inst
case object HaltInst              extends Inst
case class StoreInst(idx: Int)    extends Inst with OperandInst
case class FetchInst(idx: Int)    extends Inst with OperandInst
case object LtInst                extends Inst
case object GtInst                extends Inst
case object LeInst                extends Inst
case object GeInst                extends Inst
case object NeInst                extends Inst
case object EqInst                extends Inst
case class JzInst(var disp: Int)  extends JumpInst
case object AddInst               extends Inst
case object SubInst               extends Inst
case object MulInst               extends Inst
case object DivInst               extends Inst
case object ModInst               extends Inst
case object AndInst               extends Inst
case object OrInst                extends Inst
case object NegInst               extends Inst
case object NotInst               extends Inst
case class JmpInst(var disp: Int) extends JumpInst
