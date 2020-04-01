package xyz.hyperreal.rosettacodeCompiler

import java.io.ByteArrayOutputStream

import scala.collection.mutable
import scala.io.Source

object Main extends App {

  val symbols =
    Map(
      "*" -> "Op_multiply",
      "/" -> "Op_divide"
    )

  val delimiters =
    Map(
      '(' -> "LeftParen",
      ')' -> "RightParen",
      '{' -> "LeftBrace",
      '}' -> "RightBrace",
      ';' -> "Semicolon",
      ',' -> "Comma"
    )

  val keywords =
    Map(
      "if"    -> "Keyword_if",
      "else"  -> "Keyword_else",
      "while" -> "Keyword_while",
      "print" -> "Keyword_print",
      "putc"  -> "Keyword_putc"
    )
  val alpha        = ('a' to 'z' toSet) ++ ('A' to 'Z')
  val numeric      = '0' to '9' toSet
  val alphanumeric = alpha ++ numeric
  val identifiers  = StartRestToken("Identifier", alpha, alphanumeric)
  val integers     = SimpleToken("Integer", numeric, alpha, "alpha characters may not follow right after a number")

  val characters =
    DelimitedToken("Integer", '\'', "[^'\n]|\\n|\\\\" r, "invalid character literal", "unclosed character literal")

  val strings =
    DelimitedToken("String", '"', "[^\"\n]*" r, "invalid string literal", "unclosed string literal")

  val src =
    """
      |(asdf) /* qwer */
      |* while
      |"zxcv" 'a' 4576
      |""".trim.stripMargin

  new LexicalAnalyzer(4, symbols, delimiters, keywords, "End_of_input", identifiers, integers, characters, strings)
    .fromString(src)

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

//  val code =
//    capture(CodeGenerator.fromString("""
//                                       |Sequence
//                                       |Sequence
//                                       |;
//                                       |Assign
//                                       |Identifier    count
//                                       |Integer       1
//                                       |While
//                                       |Less
//                                       |Identifier    count
//                                       |Integer       10
//                                       |Sequence
//                                       |Sequence
//                                       |;
//                                       |Sequence
//                                       |Sequence
//                                       |Sequence
//                                       |;
//                                       |Prts
//                                       |String        "count is: "
//                                       |;
//                                       |Prti
//                                       |Identifier    count
//                                       |;
//                                       |Prts
//                                       |String        "\n"
//                                       |;
//                                       |Assign
//                                       |Identifier    count
//                                       |Add
//                                       |Identifier    count
//                                       |Integer       1
//                                       |""".trim.stripMargin))

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

//  val code =
//    capture(CodeGenerator.fromString("""
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |;
//        |Assign
//        |Identifier    count
//        |Integer       1
//        |Assign
//        |Identifier    n
//        |Integer       1
//        |Assign
//        |Identifier    limit
//        |Integer       100
//        |While
//        |Less
//        |Identifier    n
//        |Identifier    limit
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |;
//        |Assign
//        |Identifier    k
//        |Integer       3
//        |Assign
//        |Identifier    p
//        |Integer       1
//        |Assign
//        |Identifier    n
//        |Add
//        |Identifier    n
//        |Integer       2
//        |While
//        |And
//        |LessEqual
//        |Multiply
//        |Identifier    k
//        |Identifier    k
//        |Identifier    n
//        |Identifier    p
//        |Sequence
//        |Sequence
//        |;
//        |Assign
//        |Identifier    p
//        |NotEqual
//        |Multiply
//        |Divide
//        |Identifier    n
//        |Identifier    k
//        |Identifier    k
//        |Identifier    n
//        |Assign
//        |Identifier    k
//        |Add
//        |Identifier    k
//        |Integer       2
//        |If
//        |Identifier    p
//        |If
//        |Sequence
//        |Sequence
//        |;
//        |Sequence
//        |Sequence
//        |;
//        |Prti
//        |Identifier    n
//        |;
//        |Prts
//        |String        " is prime\n"
//        |;
//        |Assign
//        |Identifier    count
//        |Add
//        |Identifier    count
//        |Integer       1
//        |;
//        |Sequence
//        |Sequence
//        |Sequence
//        |;
//        |Prts
//        |String        "Total primes found: "
//        |;
//        |Prti
//        |Identifier    count
//        |;
//        |Prts
//        |String        "\n"
//        |;
//        |""".trim.stripMargin))

//  val code =
//    capture(CodeGenerator.fromString("""
//        |Sequence
//        |;
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |;
//        |Assign
//        |Identifier     left_edge
//        |Negate
//        |Integer        420
//        |;
//        |Assign
//        |Identifier     right_edge
//        |Integer        300
//        |Assign
//        |Identifier     top_edge
//        |Integer        300
//        |Assign
//        |Identifier     bottom_edge
//        |Negate
//        |Integer        300
//        |;
//        |Assign
//        |Identifier     x_step
//        |Integer        7
//        |Assign
//        |Identifier     y_step
//        |Integer        15
//        |Assign
//        |Identifier     max_iter
//        |Integer        200
//        |Assign
//        |Identifier     y0
//        |Identifier     top_edge
//        |While
//        |Greater
//        |Identifier     y0
//        |Identifier     bottom_edge
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |;
//        |Assign
//        |Identifier     x0
//        |Identifier     left_edge
//        |While
//        |Less
//        |Identifier     x0
//        |Identifier     right_edge
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |;
//        |Assign
//        |Identifier     y
//        |Integer        0
//        |Assign
//        |Identifier     x
//        |Integer        0
//        |Assign
//        |Identifier     the_char
//        |Integer        32
//        |Assign
//        |Identifier     i
//        |Integer        0
//        |While
//        |Less
//        |Identifier     i
//        |Identifier     max_iter
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |Sequence
//        |;
//        |Assign
//        |Identifier     x_x
//        |Divide
//        |Multiply
//        |Identifier     x
//        |Identifier     x
//        |Integer        200
//        |Assign
//        |Identifier     y_y
//        |Divide
//        |Multiply
//        |Identifier     y
//        |Identifier     y
//        |Integer        200
//        |If
//        |Greater
//        |Add
//        |Identifier     x_x
//        |Identifier     y_y
//        |Integer        800
//        |If
//        |Sequence
//        |Sequence
//        |Sequence
//        |;
//        |Assign
//        |Identifier     the_char
//        |Add
//        |Integer        48
//        |Identifier     i
//        |If
//        |Greater
//        |Identifier     i
//        |Integer        9
//        |If
//        |Sequence
//        |;
//        |Assign
//        |Identifier     the_char
//        |Integer        64
//        |;
//        |Assign
//        |Identifier     i
//        |Identifier     max_iter
//        |;
//        |Assign
//        |Identifier     y
//        |Add
//        |Divide
//        |Multiply
//        |Identifier     x
//        |Identifier     y
//        |Integer        100
//        |Identifier     y0
//        |Assign
//        |Identifier     x
//        |Add
//        |Subtract
//        |Identifier     x_x
//        |Identifier     y_y
//        |Identifier     x0
//        |Assign
//        |Identifier     i
//        |Add
//        |Identifier     i
//        |Integer        1
//        |Prtc
//        |Identifier     the_char
//        |;
//        |Assign
//        |Identifier     x0
//        |Add
//        |Identifier     x0
//        |Identifier     x_step
//        |Prtc
//        |Integer        10
//        |;
//        |Assign
//        |Identifier     y0
//        |Subtract
//        |Identifier     y0
//        |Identifier     y_step
//        |""".trim.stripMargin))
//
//  println(code)
//  VirtualMachine.fromString(code).run

  def capture(thunk: => Unit) = {
    val buf = new ByteArrayOutputStream

    Console.withOut(buf)(thunk)
    buf.toString
  }

}
