package xyz.hyperreal.rosettacodeCompiler

import utest._

object SampleProgramInterpreterTests extends TestSuite {

  import Testing._

  val tests = Tests {
    test("hello world") {
      assert(
        runUsingInterpreter(
          """
            |/*
            |  Hello world
            | */
            |print("Hello, World!\n");
            |""".stripMargin
        ) ==
          """
            |Hello, World!
            |""".trim.stripMargin
      )
    }

    test("phoenix number") {
      assert(
        runUsingInterpreter(
          """
            |/*
            |  Show Ident and Integers
            | */
            |phoenix_number = 142857;
            |print(phoenix_number, "\n");
            |""".stripMargin
        ) ==
          """
            |142857
            |""".trim.stripMargin
      )
    }

    test("test case 4") {
      assert(
        runUsingInterpreter(
          """
            |/*** test printing, embedded \n and comments with lots of '*' ***/
            |print(42);
            |print("\nHello World\nGood Bye\nok\n");
            |print("Print a slash n - \\n.\n");
            |""".stripMargin
        ) ==
          """
            |42
            |Hello World
            |Good Bye
            |ok
            |Print a slash n - \n.
            |""".trim.stripMargin
      )
    }

//    test("test case 4") {
//      assert(
//        runUsingInterpreter(
//          """
//            |/*** test printing, embedded \n and comments with lots of '*' ***/
//            |print(42);
//            |print("\nHello World\nGood Bye\nok\n");
//            |print("Print a slash n - \\n.\n");
//            |""".stripMargin
//        ) ==
//          """
//            |42
//            |Hello World
//            |Good Bye
//            |ok
//            |Print a slash n - \n.
//            |""".trim.stripMargin
//      )
//    }

//    test("primes") {
//      assert(
//        run("""
//                                                   |/*
//                                                   | Simple prime number generator
//                                                   | */
//                                                   |count = 1;
//                                                   |n = 1;
//                                                   |limit = 100;
//                                                   |while (n < limit) {
//                                                   |    k=3;
//                                                   |    p=1;
//                                                   |    n=n+2;
//                                                   |    while ((k*k<=n) && (p)) {
//                                                   |        p=n/k*k!=n;
//                                                   |        k=k+2;
//                                                   |    }
//                                                   |    if (p) {
//                                                   |        print(n, " is prime\n");
//                                                   |        count = count + 1;
//                                                   |    }
//                                                   |}
//                                                   |print("Total primes found: ", count, "\n");
//                                                   |""".stripMargin)) ==
//          """
//            |3 is prime
//            |5 is prime
//            |7 is prime
//            |11 is prime
//            |13 is prime
//            |17 is prime
//            |19 is prime
//            |23 is prime
//            |29 is prime
//            |31 is prime
//            |37 is prime
//            |41 is prime
//            |43 is prime
//            |47 is prime
//            |53 is prime
//            |59 is prime
//            |61 is prime
//            |67 is prime
//            |71 is prime
//            |73 is prime
//            |79 is prime
//            |83 is prime
//            |89 is prime
//            |97 is prime
//            |101 is prime
//            |Total primes found: 26
//            |""".trim.stripMargin)
//    }
  }
}
