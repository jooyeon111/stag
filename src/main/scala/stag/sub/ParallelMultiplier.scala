package stag.sub

import chisel3._

class ParallelMultiplier(numPeMultiplier: Int, portConfig: PortConfig) extends Module {

  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numPeMultiplier, SInt(portConfig.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numPeMultiplier, SInt(portConfig.bitWidthB.W)))
    val output: Vec[SInt] = Output(Vec(numPeMultiplier, SInt((portConfig.bitWidthA + portConfig.bitWidthB).W)))
  })

  for(i <- 0 until numPeMultiplier)
    io.output(i) := RegNext(io.inputA(i) * io.inputB(i), 0.S)

}
