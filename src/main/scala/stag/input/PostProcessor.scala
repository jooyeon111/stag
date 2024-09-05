package stag.input

import chisel3._
import chisel3.util.ShiftRegister

class PostProcessor(groupPeRow: Int, vectorPeRow: Int, portBitWidth: Int) extends Module {

  val numPort = groupPeRow * vectorPeRow

  val io = IO(new Bundle {
    val input: Vec[SInt] = Input(Vec(numPort, SInt(portBitWidth.W)))
    val output: Vec[SInt] = Output(Vec(numPort, SInt(portBitWidth.W)))
  })

  for (r <- 0 until groupPeRow)
    for (a <- 0 until vectorPeRow){

      val outputIndex = r * vectorPeRow + a
      val depth = groupPeRow - outputIndex
      io.output(outputIndex) := ShiftRegister(io.input(outputIndex), depth, 0.S, true.B)

    }

}
