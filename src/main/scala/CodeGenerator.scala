package xyz.hyperreal.rosettacodeCompiler

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}
import scala.io.Source

object CodeGenerator {

  def generate(ast: Source) = {
    val vars    = new LinkedHashMap[String, Int]
    val strings = new ArrayBuffer[String]
    val code    = new ArrayBuffer[Inst]
    val it      = ast.getLines

    generate

    def generate =
      if (it.hasNext)
        it.next match {
          case "Sequence" =>
          case ";"        =>
          case "Assign"   =>
        }

  }

}

abstract class Inst
abstract class JumpInst { var disp: Int }
case class PushInst(n: Int)       extends Inst
case object PrtsInst              extends Inst
case object PrtiInst              extends Inst
case object PrtcInst              extends Inst
case object HaltInst              extends Inst
case class StoreInst(idx: Int)    extends Inst
case class FetchInst(idx: Int)    extends Inst
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
