package xyz.hyperreal.rosettacodeCompiler

object Opcodes {

  val FETCH: Byte = 0
  val STORE: Byte = 1
  val PUSH: Byte  = 2
  val JMP: Byte   = 3
  val JZ: Byte    = 4
  val ADD: Byte   = 5
  val SUB: Byte   = 6
  val MUL: Byte   = 7
  val DIV: Byte   = 8
  val MOD: Byte   = 9
  val LT: Byte    = 10
  val GT: Byte    = 11
  val LE: Byte    = 12
  val GE: Byte    = 13
  val EQ: Byte    = 14
  val NE: Byte    = 15
  val AND: Byte   = 16
  val OR: Byte    = 17
  val NEG: Byte   = 18
  val NOT: Byte   = 19
  val PRTC: Byte  = 20
  val PRTI: Byte  = 21
  val PRTS: Byte  = 22
  val HALT: Byte  = 23

}
