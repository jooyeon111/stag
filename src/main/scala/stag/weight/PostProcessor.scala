package stag.weight

import chisel3._
import chisel3.util.ShiftRegister

class PostProcessor(arrayCol: Int, blockCol: Int, portBitWidth: Int) extends Module{

  val numPort: Int = arrayCol * blockCol

  val io = IO(new Bundle {
    val input: Vec[SInt] = Input(Vec(numPort, SInt(portBitWidth.W)))
    val output: Vec[SInt] = Output(Vec(numPort, SInt(portBitWidth.W)))
  })

  for ( i <- 0 until arrayCol)
    for (j <- 0 until blockCol){

      val index = i * blockCol + j
      val depth = arrayCol - index

      io.output(index) := ShiftRegister(io.input(index), depth, 0.S, true.B)

    }

}
