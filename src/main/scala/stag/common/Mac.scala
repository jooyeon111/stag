package stag.common

import chisel3._

class Mac(numPeMultiplier: Int, portBitWidth: PortBitWidth) extends Module {

  val multiplier = Module(new ParallelMultiplier(numPeMultiplier, portBitWidth))
  val adderTree = Module(new AdderTree(numPeMultiplier, portBitWidth))

  val io = IO (new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numPeMultiplier, SInt(portBitWidth.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numPeMultiplier, SInt(portBitWidth.bitWidthB.W)))
    val output: SInt = Output(SInt(portBitWidth.bitWidthC.W))
  })

  multiplier.io.inputA := io.inputA
  multiplier.io.inputB := io.inputB
  adderTree.io.input := multiplier.io.output
  io.output := adderTree.io.output

}
