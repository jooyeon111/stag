package stag.common

import chisel3._

class Mac[T <: Data](
  numPeMultiplier: Int,
  inputTypeA: T,
  inputTypeB: T,
  multiplierOutputType: T,
  adderTreeOutputType: T,
)( implicit ev: Arithmetic[T]) extends Module {

  require(numPeMultiplier >= 2, " At least 2 number of multipliers are needed for multiply and accumulation logic")

  val io = IO (new Bundle {
    val inputA = Input(Vec(numPeMultiplier, inputTypeA))
    val inputB = Input(Vec(numPeMultiplier, inputTypeB))
    val output = Output(adderTreeOutputType)
  })

  val multiplier = Module(new ParallelMultiplier(numPeMultiplier, inputTypeA, inputTypeB, multiplierOutputType))
  val adderTree = Module(new AdderTree(numPeMultiplier, multiplierOutputType, adderTreeOutputType))

  multiplier.io.inputA := io.inputA
  multiplier.io.inputB := io.inputB
  adderTree.io.input := multiplier.io.output
  io.output := adderTree.io.output

}
