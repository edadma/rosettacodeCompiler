package xyz.hyperreal.rosettacodeCompiler

import java.io.{BufferedReader, FileReader, Reader, StringReader}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import Opcodes._

object VirtualMachine {

  private val HEADER_REGEX = "Datasize: ([0-9]+) Strings: ([0-9]+)" r
  private val STRING_REGEX = "\"([^\"]*)\"" r
  private val PUSH_REGEX   = " *[0-9]+ push +([0-9]+)" r
  private val PRTS_REGEX   = " *[0-9]+ prts" r
  private val PRTI_REGEX   = " *[0-9]+ prti" r
  private val PRTC_REGEX   = " *[0-9]+ prtc" r
  private val HALT_REGEX   = " *[0-9]+ halt" r
  private val STORE_REGEX  = " *[0-9]+ store +\\[([0-9]+)\\]" r
  private val FETCH_REGEX  = " *[0-9]+ fetch +\\[([0-9]+)\\]" r
  private val LT_REGEX     = " *[0-9]+ lt" r
  private val GT_REGEX     = " *[0-9]+ gt" r
  private val LE_REGEX     = " *[0-9]+ le" r
  private val GE_REGEX     = " *[0-9]+ ge" r
  private val NE_REGEX     = " *[0-9]+ ne" r
  private val EQ_REGEX     = " *[0-9]+ eq" r
  private val JZ_REGEX     = " *[0-9]+ jz +\\((-?[0-9]+)\\) [0-9]+" r
  private val ADD_REGEX    = " *[0-9]+ add" r
  private val SUB_REGEX    = " *[0-9]+ sub" r
  private val MUL_REGEX    = " *[0-9]+ mul" r
  private val DIV_REGEX    = " *[0-9]+ div" r
  private val MOD_REGEX    = " *[0-9]+ mod" r
  private val AND_REGEX    = " *[0-9]+ and" r
  private val OR_REGEX     = " *[0-9]+ or" r
  private val NOT_REGEX    = " *[0-9]+ not" r
  private val NEG_REGEX    = " *[0-9]+ neg" r
  private val JMP_REGEX    = " *[0-9]+ jmp +\\((-?[0-9]+)\\) [0-9]+" r

  def apply(file: String) = loadFromFile(file)

  def loadFromFile(file: String) = loadFromReader(new FileReader(file))

  def loadFromString(src: String) = loadFromReader(new StringReader(src))

  def loadFromReader(r: Reader) = {
    val in = new BufferedReader(r)
    val vm =
      in.readLine match {
        case HEADER_REGEX(datasize, stringsize) =>
          val strings =
            for (_ <- 1 to stringsize.toInt)
              yield
                in.readLine match {
                  case STRING_REGEX(s) =>
                    s.replace("\\n", "\n").replace("\\r", "\r").replace("\\t", "\t").replace("\\\\", "\\")
                  case null => sys.error("expected string constant but encountered end of input")
                  case s    => sys.error(s"expected string constant: $s")
                }
          var line: String = null
          val code         = new ArrayBuffer[Byte]

          def addShort(a: Int) = {
            code += (a >> 8).toByte
            code += a.toByte
          }

          def addInst(opcode: Byte, operand: String) = {
            val opint = operand.toInt

            code += opcode
            addShort(opint >> 16)
            addShort(opint)
          }

          while ({ line = in.readLine; line ne null }) line match {
            case PUSH_REGEX(n)    => addInst(PUSH, n)
            case PRTS_REGEX()     => code += PRTS
            case PRTI_REGEX()     => code += PRTI
            case PRTC_REGEX()     => code += PRTC
            case HALT_REGEX()     => code += HALT
            case STORE_REGEX(idx) => addInst(STORE, idx)
            case FETCH_REGEX(idx) => addInst(FETCH, idx)
            case LT_REGEX()       => code += LT
            case GT_REGEX()       => code += GT
            case LE_REGEX()       => code += LE
            case GE_REGEX()       => code += GE
            case NE_REGEX()       => code += NE
            case EQ_REGEX()       => code += EQ
            case JZ_REGEX(disp)   => addInst(JZ, disp)
            case ADD_REGEX()      => code += ADD
            case SUB_REGEX()      => code += SUB
            case MUL_REGEX()      => code += MUL
            case DIV_REGEX()      => code += DIV
            case MOD_REGEX()      => code += MOD
            case AND_REGEX()      => code += AND
            case OR_REGEX()       => code += OR
            case NOT_REGEX()      => code += NOT
            case NEG_REGEX()      => code += NEG
            case JMP_REGEX(disp)  => addInst(JMP, disp)
          }

          new VirtualMachine(code, datasize.toInt, strings)
        case _ => sys.error("expected header")
      }

    in.close
    vm
  }

}

class VirtualMachine(code: IndexedSeq[Byte], datasize: Int, strings: IndexedSeq[String]) {

  var pc      = 0
  val stack   = new mutable.ArrayStack[Int]
  val data    = new Array[Int](datasize)
  var running = false

  def getByte = {
    val byte = code(pc) & 0xFF

    pc += 1
    byte
  }

  def getShort = getByte << 8 | getByte

  def getInt = getShort << 16 | getShort

  def pushBoolean(b: Boolean) = stack push (if (b) 1 else 0)

  def popBoolean = if (stack.pop != 0) true else false

  def operator(f: (Int, Int) => Int) = {
    val y = stack.pop

    stack.push(f(stack.pop, y))
  }

  def relation(r: (Int, Int) => Boolean) = {
    val y = stack.pop

    pushBoolean(r(stack.pop, y))
  }

  def connective(c: (Boolean, Boolean) => Boolean) = pushBoolean(c(popBoolean, popBoolean))

  def execute: Unit = {
    val opcode = getByte

    opcode match {
      case FETCH => stack push data(getInt)
      case STORE => data(getInt) = stack.pop
      case PUSH  => stack push getInt
      case JMP   => pc = pc + getInt
      case JZ    => if (stack.pop == 0) pc = pc + getInt else pc += 4
      case ADD   => operator(_ + _)
      case SUB   => operator(_ - _)
      case MUL   => operator(_ * _)
      case DIV   => operator(_ / _)
      case MOD   => operator(_ % _)
      case LT    => relation(_ < _)
      case GT    => relation(_ > _)
      case LE    => relation(_ <= _)
      case GE    => relation(_ >= _)
      case EQ    => relation(_ == _)
      case NE    => relation(_ != _)
      case AND   => connective(_ && _)
      case OR    => connective(_ || _)
      case NEG   => stack push -stack.pop
      case NOT   => pushBoolean(!popBoolean)
      case PRTC  => print(stack.pop.toChar)
      case PRTI  => print(stack.pop)
      case PRTS  => print(strings(stack.pop))
      case HALT  => running = false
    }
  }

  def run = {
    pc = 0
    stack.clear
    running = true

    for (i <- data.indices) data(i) = 0

    while (running) execute
  }

}
