package stag.weight

import chisel3._
import chisel3.util.ShiftRegister

class PostProcessor(groupPeCol: Int, vectorPeCol: Int, portBitWidth: Int) extends Module{

  val numPort: Int = groupPeCol * vectorPeCol

  val io = IO(new Bundle {
    val input: Vec[SInt] = Input(Vec(numPort, SInt(portBitWidth.W)))
    val output: Vec[SInt] = Output(Vec(numPort, SInt(portBitWidth.W)))
  })

  for (c <- 0 until groupPeCol)
    for (b <- 0 until vectorPeCol){

      val outputIndex = c * vectorPeCol + b
      val depth = groupPeCol - outputIndex
      io.output(outputIndex) := ShiftRegister(io.input(outputIndex), depth, 0.S, true.B)

    }

}
