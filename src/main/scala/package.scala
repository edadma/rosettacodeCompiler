package xyz.hyperreal

import java.io.ByteArrayOutputStream

package object rosettacodeCompiler {

  def capture(thunk: => Unit) = {
    val buf = new ByteArrayOutputStream

    Console.withOut(buf)(thunk)
    buf.toString
  }

}
