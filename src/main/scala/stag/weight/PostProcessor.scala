package stag.weight

import chisel3._
import chisel3.util.ShiftRegister

class PostProcessor(arrayCol: Int, blockCol: Int, portBitWidth: Int) extends Module{

  val numPort: Int = arrayCol * blockCol

  val io = IO(new Bundle {
    val input: Vec[SInt] = Input(Vec(numPort, SInt(portBitWidth.W)))
    val output: Vec[SInt] = Output(Vec(numPort, SInt(portBitWidth.W)))
  })

  for (c <- 0 until arrayCol)
    for (b <- 0 until blockCol){

      val outputIndex = c * blockCol + b
      val depth = arrayCol - outputIndex
      io.output(outputIndex) := ShiftRegister(io.input(outputIndex), depth, 0.S, true.B)

    }

}
