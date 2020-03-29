package xyz.hyperreal.rosettacodeCompiler

import scala.collection.mutable

class VirtualMachine(code: IndexedSeq[Byte], data: mutable.IndexedSeq[Int], strings: IndexedSeq[String]) {

  var pc      = 0
  val stack   = new mutable.ArrayStack[Int]
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

    import Opcodes._

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
    mutable.IndexedSeq.fill(data.size)(0)
    pc = 0
    stack.clear
    running = true

    while (running) execute
  }

}
