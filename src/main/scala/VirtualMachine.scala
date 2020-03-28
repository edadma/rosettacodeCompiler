package xyz.hyperreal.rosettacodeCompiler

import scala.collection.mutable

class VirtualMachine(code: IndexedSeq[Byte], data: IndexedSeq[Long]) {

  var pc    = 0
  val stack = new mutable.ArrayStack[Int]

  def getByte = {
    val byte = code(pc) & 0xFF

    pc += 1
    byte
  }

  def getShort = { getByte << 8 | getByte }

  def getInt = { getShort << 16 | getShort }

  def execute: Unit = {
    val opcode = getByte
  }

}
