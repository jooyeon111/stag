package stag.input

import chisel3._
import chisel3.util.ShiftRegister

class PostProcessor(arrayRow: Int, blockRow: Int, portBitWidth: Int) extends Module {

  val numPort = arrayRow * blockRow

  val io = IO(new Bundle {
    val input: Vec[SInt] = Input(Vec(numPort, SInt(portBitWidth.W)))
    val output: Vec[SInt] = Output(Vec(numPort, SInt(portBitWidth.W)))
  })

  for (r <- 0 until arrayRow)
    for (a <- 0 until blockRow){

      val outputIndex = r * blockRow + a
      val depth = arrayRow - outputIndex
      io.output(outputIndex) := ShiftRegister(io.input(outputIndex), depth, 0.S, true.B)

    }

}
