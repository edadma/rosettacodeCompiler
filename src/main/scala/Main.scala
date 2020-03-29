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

  VirtualMachine.loadFromString("""
      |Datasize: 1 Strings: 2
      |"count is: "
      |"\n"
      |    0 push  1
      |    5 store [0]
      |   10 fetch [0]
      |   15 push  10
      |   20 lt
      |   21 jz     (43) 65
      |   26 push  0
      |   31 prts
      |   32 fetch [0]
      |   37 prti
      |   38 push  1
      |   43 prts
      |   44 fetch [0]
      |   49 push  1
      |   54 add
      |   55 store [0]
      |   60 jmp    (-51) 10
      |   65 halt
      |""".trim.stripMargin).run

//  VirtualMachine.loadFromString("""
//      |Datasize: 1 Strings: 2
//      |"Hello world! "
//      |"\n"
//      | 0 push 0
//      | 5 prts
//      | 0 push 1
//      | 0 push 2
//      | 0 lt
//      | 0 jz (20) 0
//      | 6 push 5
//      | 0 store [0]
//      | 0 fetch [0]
//      | 0 prti
//      | 0 push 1
//      | 0 prts
//      | 6 halt
//      |""".trim.stripMargin).run

}
