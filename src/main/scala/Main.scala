package xyz.hyperreal.rosettacodeCompiler

import java.io.ByteArrayOutputStream

import scala.collection.mutable
import scala.io.Source

object Main extends App {

//  VirtualMachine.fromString("""
//      |Datasize: 1 Strings: 2
//      |"count is: "
//      |"\n"
//      |    0 push  1
//      |    5 store [0]
//      |   10 fetch [0]
//      |   15 push  10
//      |   20 lt
//      |   21 jz     (43) 65
//      |   26 push  0
//      |   31 prts
//      |   32 fetch [0]
//      |   37 prti
//      |   38 push  1
//      |   43 prts
//      |   44 fetch [0]
//      |   49 push  1
//      |   54 add
//      |   55 store [0]
//      |   60 jmp    (-51) 10
//      |   65 halt
//      |""".trim.stripMargin).run

  val code =
    capture(CodeGenerator.fromString("""
                                       |Sequence
                                       |Sequence
                                       |;
                                       |Assign
                                       |Identifier    count
                                       |Integer       1
                                       |While
                                       |Less
                                       |Identifier    count
                                       |Integer       10
                                       |Sequence
                                       |Sequence
                                       |;
                                       |Sequence
                                       |Sequence
                                       |Sequence
                                       |;
                                       |Prts
                                       |String        "count is: "
                                       |;
                                       |Prti
                                       |Identifier    count
                                       |;
                                       |Prts
                                       |String        "\n"
                                       |;
                                       |Assign
                                       |Identifier    count
                                       |Add
                                       |Identifier    count
                                       |Integer       1
                                       |""".trim.stripMargin))

//  val code =
//    capture(CodeGenerator.fromString("""
//                                       |If
//                                       |Integer 1
//                                       |If
//                                       |Prti
//                                       |Integer       345
//                                       |;
//                                       |;
//                                       |""".trim.stripMargin))

//  val code =
//    capture(CodeGenerator.fromString("""
//                                       |If
//                                       |Integer 0
//                                       |If
//                                       |Prti
//                                       |Integer       345
//                                       |;
//                                       |Prti
//                                       |Integer       678
//                                       |;
//                                      """.trim.stripMargin))

  println(code)
  VirtualMachine.fromString(code).run

  def capture(thunk: => Unit) = {
    val buf = new ByteArrayOutputStream

    Console.withOut(buf)(thunk)
    buf.toString
  }

}
