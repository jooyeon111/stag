package stag.output

import chisel3._
import chisel3.util.ShiftRegister
import stag.common.Arithmetic

class DeskewBuffer[T <: Data](
  groupPeRow: Int,
  groupPeCol: Int,
  vectorPeRow: Int,
  vectorPeCol: Int,
  portType: T
)( implicit ev: Arithmetic[T]) extends Module {

  val numPort = (groupPeRow + groupPeCol - 1) * vectorPeRow * vectorPeCol

  val io = IO(new Bundle {
    val input = Input(Vec(numPort, portType))
    val output = Output(Vec(numPort, portType))
  })

  for( i <- 0 until groupPeRow + groupPeCol - 1)
    for(j <- 0 until vectorPeRow * vectorPeCol){

      val index = i * vectorPeRow*vectorPeCol + j
      val depth = if( i < groupPeRow - 1 ){groupPeRow - i - 1} else 0

      io.output(index) := ShiftRegister(io.input(index), depth, ev.zero(portType.getWidth), true.B)

    }
}
