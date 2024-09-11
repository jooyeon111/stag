package stag.common

import chisel3._

class AdderTree[InputType <: Data, OutputType <: Data](
  numPeMultiplier: Int,
  inputType: InputType,
)(implicit ev: AdderTreeOperation[InputType, OutputType]) extends Module{

  val outputType = ev.getOutputType(Seq.fill(numPeMultiplier)(inputType))

  val io = IO(new Bundle {
    val input = Input(Vec(numPeMultiplier, inputType))
    val output = Output(outputType)
  })

  io.output := io.input.reduceTree( (input0 , input1 ) => RegNext(ev.add(input0, input1)))

}
