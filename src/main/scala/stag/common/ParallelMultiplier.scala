package stag.common

import chisel3._

class ParallelMultiplier(numPeMultiplier: Int, portBitWidth: PortBitWidth) extends Module {

  val outputPortBitwidth = portBitWidth.bitWidthA + portBitWidth.bitWidthB

  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numPeMultiplier, SInt(portBitWidth.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numPeMultiplier, SInt(portBitWidth.bitWidthB.W)))
    val output: Vec[SInt] = Output(Vec(numPeMultiplier, SInt(outputPortBitwidth.W)))
  })

  for(i <- 0 until numPeMultiplier)
    io.output(i) := RegNext(io.inputA(i) * io.inputB(i), 0.S)

}
