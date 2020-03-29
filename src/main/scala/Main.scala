package xyz.hyperreal.rosettacodeCompiler

import scala.collection.mutable

object Main extends App {

//  import Opcodes._
//
//  val code    = IndexedSeq[Byte](PUSH, 0, 0, 0, 0, PRTS, HALT)
//  val data    = mutable.IndexedSeq[Int]()
//  val strings = IndexedSeq("Hello world!\n")
//
//  new VirtualMachine(code, 0, strings).run

//  VirtualMachine.loadFromString("""
//      |Datasize: 1 Strings: 2
//      |"count is: "
//      |"\n"
//      |""".trim.stripMargin)

  VirtualMachine.loadFromString("""
      |Datasize: 0 Strings: 1
      |"Hello world!\n"
      | 0 push 0
      | 5 prts
      | 6 halt
      |""".trim.stripMargin).run

}
