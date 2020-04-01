package xyz.hyperreal.rosettacodeCompiler

import utest._

object LexicalAnalyzerTests extends TestSuite {

  import Testing._

  val tests = Tests {
    test("hello world") {
      assert(
        capture(LexicalAnalyzer.apply.fromString("""
          |/*
          |  Hello world
          | */
          |print("Hello, World!\n");
          |""".trim.stripMargin)) ==
          """
          |    4      1 Keyword_print
          |    4      6 LeftParen
          |    4      7 String         "Hello, World!\n"
          |    4     24 RightParen
          |    4     25 Semicolon
          |    5      1 End_of_input
          |""".trim.stripMargin)
    }

    test("phoenix number") {
      assert(
        capture(LexicalAnalyzer.apply.fromString("""
                                                   |/*
                                                   |  Show Ident and Integers
                                                   | */
                                                   |phoenix_number = 142857;
                                                   |print(phoenix_number, "\n");
                                                   |""".trim.stripMargin)) ==
          """
            |    4      1 Identifier     phoenix_number
            |    4     16 Op_assign
            |    4     18 Integer        142857
            |    4     24 Semicolon
            |    5      1 Keyword_print
            |    5      6 LeftParen
            |    5      7 Identifier     phoenix_number
            |    5     21 Comma
            |    5     23 String         "\n"
            |    5     27 RightParen
            |    5     28 Semicolon
            |    6      1 End_of_input
            |""".trim.stripMargin)
    }
  }

}
