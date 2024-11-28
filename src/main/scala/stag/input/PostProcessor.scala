package stag.input

import chisel3._
import chisel3.util.ShiftRegister
import stag.common.{Arithmetic, VerilogNaming}

class PostProcessor[T <: Data](
  groupPeRow: Int,
  vectorPeRow: Int,
  portType: T
)(implicit ev: Arithmetic[T]) extends Module with VerilogNaming{

  override val desiredName:String = camelToSnake(this.getClass.getSimpleName)

  val numPort = groupPeRow * vectorPeRow

  val io = IO(new Bundle {
    val input = Input(Vec(numPort, portType))
    val output = Output(Vec(numPort, portType))
  })

  for (r <- 0 until groupPeRow)
    for (a <- 0 until vectorPeRow){

      val outputIndex = r * vectorPeRow + a
      val depth = groupPeRow - outputIndex
      io.output(outputIndex) := ShiftRegister(io.input(outputIndex), depth, ev.zero(portType.getWidth), true.B)

    }

}
