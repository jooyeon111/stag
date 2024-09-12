package stag.weight

import chisel3._
import chisel3.util.ShiftRegister
import stag.common.{Arithmetic, PortConfig}

class PostProcessor[T <: Data](
  groupPeCol: Int,
  vectorPeCol: Int,
  portType: T
)(implicit ev: Arithmetic[T]) extends Module{

  val numPort: Int = groupPeCol * vectorPeCol

  val io = IO(new Bundle {
    val input = Input(Vec(numPort, portType))
    val output = Output(Vec(numPort, portType))
  })

  for (c <- 0 until groupPeCol)
    for (b <- 0 until vectorPeCol){

      val outputIndex = c * vectorPeCol + b
      val depth = groupPeCol - outputIndex
      io.output(outputIndex) := ShiftRegister(io.input(outputIndex), depth, ev.zero(portType.getWidth), true.B)

    }

}
