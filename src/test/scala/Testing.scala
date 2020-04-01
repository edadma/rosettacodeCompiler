package xyz.hyperreal.rosettacodeCompiler

import java.io.ByteArrayOutputStream

object Testing {

  def capture(thunk: => Unit) = {
    val buf = new ByteArrayOutputStream

    Console.withOut(buf)(thunk)
    buf.toString
  }

}
