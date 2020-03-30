package xyz.hyperreal.rosettacodeCompiler

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}
import scala.io.Source

object CodeGenerator {

  def fromStdin = fromSource(Source.stdin)

  def fromString(src: String) = fromSource(Source.fromString(src))

  def fromSource(ast: Source) = {
    val vars              = new LinkedHashMap[String, Int]
    val strings           = new ArrayBuffer[String]
    val code              = new ArrayBuffer[Inst]
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

    def variable(name: String) =
      vars get name match {
        case None =>
          val idx = vars.size

          vars(name) = idx
          idx
        case Some(idx) => idx
      }

    var loc = 0

    def add(inst: Inst) = {
      loc += (if (inst.isInstanceOf[OperandInst]) 5 else 1)
      code += inst
    }

    generate
    add(HaltInst)
    println(s"Datasize: ${vars.size} Strings: ${strings.length}")

    for (s <- strings)
      println(s)

    loc = 0

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
        case LtInst         => "lt"
        case GtInst         => "gt"
        case LeInst         => "le"
        case GeInst         => "ge"
        case NeInst         => "ne"
        case EqInst         => "eq"
        case JzInst(disp)   => s"jz    ($disp) ${loc + disp + 1}"
        case AddInst        => "add"
        case SubInst        => "sub"
        case MulInst        => "mul"
        case DivInst        => "div"
        case ModInst        => "mod"
        case AndInst        => "and"
        case OrInst         => "or"
        case NegInst        => "neg"
        case NotInst        => "not"
        case JmpInst(disp)  => s"jmp   ($disp) ${loc + disp + 1}"
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
              case Array("Identifier", name: String) =>
                variable(name)
              case l => sys.error(s"expected identifier: $l")
            }

          generate
          add(StoreInst(idx))
        case Array("Identifier", name: String) => add(FetchInst(variable(name)))
        case Array("Integer", n: String)       => add(PushInst(n.toInt))
        case Array("String", s: String) =>
          add(PushInst(strings indexOf s match {
            case -1 =>
              val idx = strings.length

              strings += s
              idx
            case idx => idx
          }))
        case "If" =>
          generate

          val cond    = loc
          val condidx = code.length

          add(JzInst(0))
          s = s.tail // skip "If"
          generate

          if (s.head == ";") {
            s = s.tail
            code(condidx) = JzInst(loc - cond - 1)
          } else {
            val jump    = loc
            val jumpidx = code.length

            add(JmpInst(0))
            code(condidx) = JzInst(loc - cond - 1)
            generate
            code(jumpidx) = JmpInst(loc - jump - 1)
          }
        case "While" =>
          val start = loc

          generate

          val cond    = loc
          val condidx = code.length

          add(JzInst(0))
          generate
          add(JmpInst(start - loc - 1))
          code(condidx) = JzInst(loc - cond - 1)
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
