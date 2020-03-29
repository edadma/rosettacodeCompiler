package xyz.hyperreal.rosettacodeCompiler

import scala.collection.mutable

object Main extends App {

  import Opcodes._

  val code    = IndexedSeq[Byte](PUSH, 0, 0, 0, 0, PRTS, HALT)
  val data    = mutable.IndexedSeq[Int]()
  val strings = IndexedSeq("Hello world!\n")

  new VirtualMachine(code, data, strings).run
}
