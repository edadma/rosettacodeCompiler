package xyz.hyperreal.rosettacodeCompiler

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}
import scala.io.Source

object CodeGenerator {

  def fromStdin = fromSource(Source.stdin)

  def fromString(src: String) = fromSource(Source.fromString(src))

  def fromSource(ast: Source) = {
    val vars              = new LinkedHashMap[String, Int]
    val strings           = new ArrayBuffer[String]
    val code_old          = new ArrayBuffer[Inst]
    val code              = new ArrayBuffer[String]
    var s: Stream[String] = ast.getLines.toStream

    def line =
      if (s.nonEmpty) {
        val n = s.head

        s = s.tail

        n.split(" +", 2) match {
          case Array(n) => n
          case a        => a
        }
      } else
        sys.error("unexpected end of AST")

    def variableIndex(name: String) =
      vars get name match {
        case None =>
          val idx = vars.size

          vars(name) = idx
          idx
        case Some(idx) => idx
      }

    def stringIndex(s: String) =
      strings indexOf s match {
        case -1 =>
          val idx = strings.length

          strings += s
          idx
        case idx => idx
      }

    var loc     = 0
    var loc_old = 0

    def add(inst: Inst) = {
      loc_old += (if (inst.isInstanceOf[OperandInst]) 5 else 1)
      code_old += inst
    }

    def addSimple(inst: String) = {
      code += f"$loc%4d $inst"
      loc += 1
    }

    def addOperand(inst: String, operand: String) = {
      code += f"$loc%4d $inst%-5s $operand"
      loc += 5
    }

    def fixup(inst: String, idx: Int, at: Int) = code(idx) = f"$at%4d $inst%-5s (${loc - at - 1}) $loc"

    generate
    add(HaltInst)
    addSimple("halt")
    println(s"Datasize: ${vars.size} Strings: ${strings.length}")

    for (s <- strings)
      println(s)

//    loc_old = 0
//
//    for (inst <- code_old) {
//      print(f"$loc_old%4d ")
//      println(inst match {
//        case PushInst(n)    => s"push  $n"
//        case PrtsInst       => "prts"
//        case PrtiInst       => "prti"
//        case PrtcInst       => "prtc"
//        case HaltInst       => "halt"
//        case StoreInst(idx) => s"store [$idx]"
//        case FetchInst(idx) => s"fetch [$idx]"
//        case LtInst         => "lt"
//        case GtInst         => "gt"
//        case LeInst         => "le"
//        case GeInst         => "ge"
//        case NeInst         => "ne"
//        case EqInst         => "eq"
//        case JzInst(disp)   => s"jz    ($disp) ${loc_old + disp + 1}"
//        case AddInst        => "add"
//        case SubInst        => "sub"
//        case MulInst        => "mul"
//        case DivInst        => "div"
//        case ModInst        => "mod"
//        case AndInst        => "and"
//        case OrInst         => "or"
//        case NegInst        => "neg"
//        case NotInst        => "not"
//        case JmpInst(disp)  => s"jmp   ($disp) ${loc_old + disp + 1}"
//      })
//
//      loc_old += (if (inst.isInstanceOf[OperandInst]) 5 else 1)
//    }
//
//    println
    println(code mkString "\n")

    def generate: Unit =
      line match {
        case "Sequence" =>
          generate
          generate
        case ";" =>
        case "Assign" =>
          val idx =
            line match {
              case Array("Identifier", name: String) =>
                variableIndex(name)
              case l => sys.error(s"expected identifier: $l")
            }

          generate
          add(StoreInst(idx))
          addOperand("store", s"[$idx]")
        case Array("Identifier", name: String) =>
          add(FetchInst(variableIndex(name)))
          addOperand("fetch", s"[${variableIndex(name)}]")
        case Array("Integer", n: String) =>
          add(PushInst(n.toInt))
          addOperand("push", s"$n")
        case Array("String", s: String) =>
          add(PushInst(stringIndex(s)))
          addOperand("push", s"${stringIndex(s)}")
        case "If" =>
          generate

          val cond    = loc
          val condidx = code.length

          add(JzInst(0))
          addOperand("", "")
          s = s.tail
          generate

          if (s.head == ";") {
            s = s.tail
            code_old(condidx) = JzInst(loc_old - cond - 1)
            fixup("jz", condidx, cond)
          } else {
            val jump    = loc
            val jumpidx = code.length

            add(JmpInst(0))
            addOperand("", "")
            code_old(condidx) = JzInst(loc_old - cond - 1)
            fixup("jz", condidx, cond)
            generate
            code_old(jumpidx) = JmpInst(loc_old - jump - 1)
            fixup("jmp", jumpidx, jump)
          }
        case "While" =>
          val start = loc

          generate

          val cond    = loc
          val condidx = code.length

          add(JzInst(0))
          addOperand("", "")
          generate
          add(JmpInst(start - loc_old - 1))
          addOperand("jmp", s"(${start - loc - 1}) $start")
          code_old(condidx) = JzInst(loc_old - cond - 1)
          fixup("jz", condidx, cond)
        case op =>
          generate
          generate
          add(
            op match {
              case "Prti"         => PrtiInst
              case "Prts"         => PrtsInst
              case "Add"          => AddInst
              case "Subtract"     => SubInst
              case "Multiply"     => MulInst
              case "Divide"       => DivInst
              case "Mod"          => ModInst
              case "Less"         => LtInst
              case "LessEqual"    => LeInst
              case "Greater"      => GtInst
              case "GreaterEqual" => GeInst
              case "Equal"        => EqInst
              case "NotEqual"     => NeInst
              case "And"          => AndInst
              case "Or"           => OrInst
            }
          )
          addSimple(
            op match {
              case "Prti"         => "prti"
              case "Prts"         => "prts"
              case "Add"          => "add"
              case "Subtract"     => "sub"
              case "Multiply"     => "mul"
              case "Divide"       => "div"
              case "Mod"          => "div"
              case "Less"         => "lt"
              case "LessEqual"    => "le"
              case "Greater"      => "gt"
              case "GreaterEqual" => "ge"
              case "Equal"        => "eq"
              case "NotEqual"     => "ne"
              case "And"          => "and"
              case "Or"           => "or"
            }
          )
      }
  }

}

abstract class Inst
abstract class OperandInst     extends Inst
case class PushInst(n: Int)    extends OperandInst
case object PrtsInst           extends Inst
case object PrtiInst           extends Inst
case object PrtcInst           extends Inst
case object HaltInst           extends Inst
case class StoreInst(idx: Int) extends OperandInst
case class FetchInst(idx: Int) extends OperandInst
case object LtInst             extends Inst
case object GtInst             extends Inst
case object LeInst             extends Inst
case object GeInst             extends Inst
case object NeInst             extends Inst
case object EqInst             extends Inst
case class JzInst(disp: Int)   extends OperandInst
case object AddInst            extends Inst
case object SubInst            extends Inst
case object MulInst            extends Inst
case object DivInst            extends Inst
case object ModInst            extends Inst
case object AndInst            extends Inst
case object OrInst             extends Inst
case object NegInst            extends Inst
case object NotInst            extends Inst
case class JmpInst(disp: Int)  extends OperandInst
