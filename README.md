rosettacodeCompiler
===================

This project provides a complete, well tested, fully working implementation of the [Rosetta Code Compiler](https://rosettacode.org/wiki/Compiler) tasks. The project was implemented using the Scala Native (v0.3.9) runtime, which means it produces a native Linux application. Currently, Scala Native only supports Linux as a target, so the app can't yet be run on Windows. 

The [Rosetta Code](https://rosettacode.org/) tasks completed are:

* [lexical analyzer](https://rosettacode.org/wiki/Compiler/lexical_analyzer)
* [syntax analyzer](https://rosettacode.org/wiki/Compiler/syntax_analyzer)
* [code generator](https://rosettacode.org/wiki/Compiler/code_generator)
* [virtual machine interpreter](https://rosettacode.org/wiki/Compiler/virtual_machine_interpreter)
* [AST interpreter](https://rosettacode.org/wiki/Compiler/AST_interpreter)


Prerequisites
-------------

Building
--------

Clone, test, and build an executable with the following commands:

```bash
git clone 
cd rosettacodeCompiler
sbt test
sbt nativeLink
```

Usage
-----

Once built, the execute can be reached by typing

    cd target/scala-2.11
    
while still in the project folder. Run the 