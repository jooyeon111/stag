package stag.output

import chisel3._
import chisel3.util.ShiftRegister

class PostProcessor(arrayRow: Int, arrayCol: Int, blockRow: Int, blockCol: Int, portBitWidth: Int) extends Module {

  val numPort = (arrayRow + arrayCol - 1) * blockRow * blockCol

  val io = IO(new Bundle {
    val input: Vec[SInt] = Input(Vec(numPort, SInt(portBitWidth.W)))
    val output: Vec[SInt] = Output(Vec(numPort, SInt(portBitWidth.W)))
  })

  //
  for( i <- 0 until arrayRow + arrayCol - 1)
    for(j <- 0 until blockRow * blockCol){

      val index = i * blockRow*blockCol + j
      val depth = if( i < arrayRow - 1 ){arrayRow - i - 1} else 0

      io.output(index) := ShiftRegister(io.input(index), depth, 0.S, true.B)

    }
}
