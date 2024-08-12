package stag.common

import chisel3._

class Mac(numPeMultiplier: Int, portConfig: PortConfig) extends Module {

  val multiplier = Module(new ParallelMultiplier(numPeMultiplier, portConfig))
  val adderTree = Module(new AdderTree(numPeMultiplier, portConfig))

  val io = IO (new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numPeMultiplier, SInt(portConfig.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numPeMultiplier, SInt(portConfig.bitWidthB.W)))
    val output: SInt = Output(SInt(portConfig.bitWidthC.W))
  })

  multiplier.io.inputA := io.inputA
  multiplier.io.inputB := io.inputB
  adderTree.io.input := multiplier.io.output
  io.output := adderTree.io.output

}
