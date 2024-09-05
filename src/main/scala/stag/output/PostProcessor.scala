package stag.output

import chisel3._
import chisel3.util.ShiftRegister

class PostProcessor(groupPeRow: Int, groupPeCol: Int, vectorPeRow: Int, vectorPeCol: Int, portBitWidth: Int) extends Module {

  val numPort = (groupPeRow + groupPeCol - 1) * vectorPeRow * vectorPeCol

  val io = IO(new Bundle {
    val input: Vec[SInt] = Input(Vec(numPort, SInt(portBitWidth.W)))
    val output: Vec[SInt] = Output(Vec(numPort, SInt(portBitWidth.W)))
  })

  //
  for( i <- 0 until groupPeRow + groupPeCol - 1)
    for(j <- 0 until vectorPeRow * vectorPeCol){

      val index = i * vectorPeRow*vectorPeCol + j
      val depth = if( i < groupPeRow - 1 ){groupPeRow - i - 1} else 0

      io.output(index) := ShiftRegister(io.input(index), depth, 0.S, true.B)

    }
}
