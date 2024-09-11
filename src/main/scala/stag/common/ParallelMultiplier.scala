package stag.common

import chisel3._

class ParallelMultiplier[InputTypeA <: Data, InputTypeB <: Data, OutputType <: Data](
  numPeMultiplier: Int,
  inputTypeA: InputTypeA,
  inputTypeB: InputTypeB,
)(implicit ev: MultiplierOperation[InputTypeA, InputTypeB, OutputType]) extends Module {

  private val outputType = ev.getOutputType(inputTypeA, inputTypeB)

  val io = IO(new Bundle {
    val inputA = Input(Vec(numPeMultiplier, inputTypeA ))
    val inputB = Input(Vec(numPeMultiplier, inputTypeB ))
    val output = Output(Vec(numPeMultiplier, outputType ))
  })

  for(i <- 0 until numPeMultiplier)
    io.output(i) := RegNext(ev.multiply(io.inputA(i), io.inputB(i)), ev.zero(outputType))

}
