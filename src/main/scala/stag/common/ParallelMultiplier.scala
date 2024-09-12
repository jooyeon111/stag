package stag.common

import chisel3._

class ParallelMultiplier[T <: Data](
  numPeMultiplier: Int,
  inputTypeA: T,
  inputTypeB: T,
  outputType: T
)(implicit ev: Arithmetic[T]) extends Module {

  val io = IO(new Bundle {
    val inputA = Input(Vec(numPeMultiplier, inputTypeA ))
    val inputB = Input(Vec(numPeMultiplier, inputTypeB ))
    val output = Output(Vec(numPeMultiplier, outputType ))
  })

  for(i <- 0 until numPeMultiplier)
    io.output(i) := RegNext(ev.multiply(io.inputA(i), io.inputB(i)), ev.zero(outputType.getWidth))

}


