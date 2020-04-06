rosettacodeCompiler
===================

This project provides a complete, well tested, fully working implementation of the [Rosetta Code Compiler](https://rosettacode.org/wiki/Compiler) tasks. The project was implemented using the [Scala Native v0.3.9](https://scala-native.readthedocs.io/en/v0.3.9-docs/) runtime, which means it produces a native executable. Currently, Scala Native doesn't support Windows as a target, so the app can only be run on Linux or MacOS. 

The [Rosetta Code](https://rosettacode.org/) tasks completed are:

* [lexical analyzer](https://rosettacode.org/wiki/Compiler/lexical_analyzer)
* [syntax analyzer](https://rosettacode.org/wiki/Compiler/syntax_analyzer)
* [code generator](https://rosettacode.org/wiki/Compiler/code_generator)
* [virtual machine interpreter](https://rosettacode.org/wiki/Compiler/virtual_machine_interpreter)
* [AST interpreter](https://rosettacode.org/wiki/Compiler/AST_interpreter)

Prerequisites
-------------

The build dependencies are
* Java 11
* sbt 1.3.8

For any Scala Native project, you have to install [Clang](https://clang.llvm.org/) and some runtime dependencies. For Ubuntu (what I'm using), type

    sudo apt install clang libunwind-dev libgc-dev libre2-dev
    
For other OS's, see Scala Native's [environment setup](https://scala-native.readthedocs.io/en/v0.3.9-docs/user/setup.html#installing-clang-and-runtime-dependencies).

Building
--------

Clone and test by typing:

```bash
git clone https://github.com/edadma/rosettacodeCompiler.git
cd rosettacodeCompiler
sbt test
```

You should see a line of output that says

    [info] Tests: 26, Passed: 26, Failed: 0
    
If you don't or if the number after `Failed` isn't 0, there's a problem.

Build an executable with

```bash
sbt nativeLink
```

Usage
-----

Once built and while still in the project folder, the executable can be reached by typing

    cd target/scala-2.11
    
To run it and show usage text, type

    ./rosettacodeCompiler-out -h
    
or for less typing

    ./r* -h
    
You should see

    Rosetta Code Compiler v0.1
    Usage: rosettacodeCompiler [options]
    
      -g, --gen      run code generator (only unless -s is used)
      -h, --help     print this usage text
      -i, --interp   run interpreter (only unless -s is used)
      -l, --lexer    run lexer only
      -p, --parser   run parser (only unless -s is used)
      -r, --run      run using virtual machine (only unless -s is used)
      -s, --source   run all prior steps
      -v, --version  print the version

If you don't, there's a problem.

Samples
--------

There's a `samples` folder with a number of sample programs. For example, to run the prime number generator program, type

    ./r* <../../samples/primes.s
    
You should see

```
3 is prime
5 is prime
7 is prime
11 is prime
13 is prime
17 is prime
19 is prime
23 is prime
29 is prime
31 is prime
37 is prime
41 is prime
43 is prime
47 is prime
53 is prime
59 is prime
61 is prime
67 is prime
71 is prime
73 is prime
79 is prime
83 is prime
89 is prime
97 is prime
101 is prime
Total primes found: 26

Completed in 0.036s
```

If you don't, there's a problem. Of course, completion times will vary.

The above command runs the program using the interpreter, and not the virtual machine, which is the default with no options. To run the same program using the virtual machine, type

    ./r* -s -r <../../samples/primes.s
    
License
-------

This project is licensed under the ISC License - see the [LICENSE.md](LICENSE) file for details.