package xyz.hyperreal.rosettacodeCompiler

import java.io.{BufferedReader, FileReader, Reader, StringReader}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import Opcodes._

object VirtualMachine {

  private val HEADER_REGEX = "Datasize: ([0-9]+) Strings: ([0-9]+)" r
  private val STRING_REGEX = "\"([^\"]*)\"" r
  private val PUSH_REGEX   = "[ ]*[0-9]+ push[ ]+([0-9]+)" r
  private val PRTS_REGEX   = "[ ]*[0-9]+ prts" r
  private val HALT_REGEX   = "[ ]*[0-9]+ halt" r

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

          def addInst(opcode: Byte, operand: Int) = {
            code += opcode
            addShort(operand >> 16)
            addShort(operand)
          }

          while ({ line = in.readLine; line ne null }) line match {
            case PUSH_REGEX(n) => addInst(PUSH, n.toInt)
            case PRTS_REGEX()  => code += PRTS
            case HALT_REGEX()  => code += HALT
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

  def getShort = { getByte << 8 | getByte }

  def getInt = { getShort << 16 | getShort }

  def pushBoolean(b: Boolean) = stack push (if (b) 1 else 0)

  def popBoolean = if (stack.pop != 0) true else false

  def execute: Unit = {
    val opcode = getByte

    opcode match {
      case FETCH => stack push data(getInt)
      case STORE => data(getInt) = stack.pop
      case PUSH  => stack push getInt
      case JMP   => pc += getInt
      case JZ    => if (stack.pop != 0) pc += getInt else pc += 4
      case ADD   => stack push (stack.pop + stack.pop)
      case SUB =>
        val y = stack pop

        stack push (stack.pop - y)
      case MUL => stack push (stack.pop * stack.pop)
      case DIV =>
        val y = stack pop

        stack push (stack.pop / y)
      case MOD =>
        val y = stack pop

        stack push (stack.pop % y)
      case LT   => pushBoolean(stack.pop >= stack.pop)
      case GT   => pushBoolean(stack.pop <= stack.pop)
      case LE   => pushBoolean(stack.pop > stack.pop)
      case GE   => pushBoolean(stack.pop < stack.pop)
      case EQ   => pushBoolean(stack.pop == stack.pop)
      case NE   => pushBoolean(stack.pop != stack.pop)
      case AND  => pushBoolean(popBoolean && popBoolean)
      case OR   => pushBoolean(popBoolean || popBoolean)
      case NEG  => stack push -stack.pop
      case NOT  => pushBoolean(!popBoolean)
      case PRTC => print(stack.pop.toChar)
      case PRTI => print(stack.pop)
      case PRTS => print(strings(stack.pop))
      case HALT => running = false
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
