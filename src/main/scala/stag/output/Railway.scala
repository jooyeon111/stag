package stag.output

import chisel3._
import chisel3.util.MuxCase
import stag.common.Arithmetic
import scala.math.{ceil, log10}

class Railway[T <: Data](
  groupPeRow: Int,
  groupPeCol: Int,
  vectorPeRow: Int,
  vectorPeCol: Int,
  portType: T
)(implicit ev: Arithmetic[T]) extends Module{

  val numInput: Int = (groupPeRow + groupPeCol - 1) * vectorPeRow * vectorPeCol
  val muxSignalBit = ceil(log10(groupPeCol.toDouble) /  log10(2.0)).toInt
  val numOutput: Int = groupPeRow * vectorPeRow * vectorPeCol

  val io = IO(new Bundle {
    val input = Input(Vec(numInput, portType))
    val control = Input(UInt(muxSignalBit.W))
    val output = Output(Vec(numOutput, portType))
  })

  val semaphore = for (i <- 0 until numOutput) yield {
    for (j <- 0 until groupPeCol)
      yield {
        (io.control(i) === j.U) -> io.input(i + (vectorPeRow * vectorPeCol * j))
      }
  }

  for ( i <- 0 until numOutput)
    io.output(i) := RegNext(MuxCase(ev.zero(portType.getWidth) , semaphore(i)), ev.zero(portType.getWidth))

}
