package stag.sub

import chisel3._
import chisel3.util.ShiftRegister

class PreProcessor(arrayConfig: Int, blockConfig: Int, peMultiplierCount: Int, skewFlag: Boolean, portBitWidth: Int) extends Module {

  val numPort: Int = arrayConfig * blockConfig * peMultiplierCount

  val io = IO(new Bundle {
    val input: Vec[SInt] = Input(Vec(numPort, SInt(portBitWidth.W)))
    val output: Vec[SInt] = Output(Vec(numPort, SInt(portBitWidth.W)))
  })

  if(skewFlag){

    for (i <- 0 until arrayConfig)
      for (j <- 0 until blockConfig * peMultiplierCount) {
        val index = i * blockConfig * peMultiplierCount + j
        val depth = i + 1
        io.output(index) := ShiftRegister(io.input(index), depth, 0.S, true.B)
      }

  } else {

    for( i <- 0 until numPort)
      io.output(i) := RegNext( io.input(i), 0.S(portBitWidth.W) )

  }


}
