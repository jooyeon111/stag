package stag.common

import chisel3._

class Mac[InputTypeA <: Data, InputTypeB <: Data, MultOutputType <: Data, AdderOutputType <: Data](
  numPeMultiplier: Int,
  inputTypeA: InputTypeA,
  inputTypeB: InputTypeB,
)( implicit
  evMult: MultiplierOperation[InputTypeA, InputTypeB, MultOutputType],
  evAdd: AdderTreeOperation[MultOutputType, AdderOutputType]
) extends Module {

  val multiplier = Module(new ParallelMultiplier(numPeMultiplier, inputTypeA, inputTypeB))
  val adderTree = Module(new AdderTree(numPeMultiplier, evMult.getOutputType(inputTypeA, inputTypeB)))

  val io = IO (new Bundle {
    val inputA = Input(Vec(numPeMultiplier, inputTypeA))
    val inputB = Input(Vec(numPeMultiplier, inputTypeB))
    val output = Output(evAdd.getOutputType(Seq.fill(numPeMultiplier)(evMult.getOutputType(inputTypeA, inputTypeB))))
  })

  multiplier.io.inputA := io.inputA
  multiplier.io.inputB := io.inputB
  adderTree.io.input := multiplier.io.output
  io.output := adderTree.io.output

}
