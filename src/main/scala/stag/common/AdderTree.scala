package stag.common

import chisel3._

class AdderTree[T <: Data](
  numPeMultiplier: Int,
  inputType: T,
  outputType: T
)(implicit ev: Arithmetic[T]) extends Module with VerilogNaming{

  override val desiredName: String = camelToSnake(name)

  val io = IO(new Bundle {
    val input = Input(Vec(numPeMultiplier, inputType))
    val output = Output(outputType)
  })

  io.output := io.input.reduceTree( (input0 , input1 ) => RegNext(ev.add(input0, input1)))

}
