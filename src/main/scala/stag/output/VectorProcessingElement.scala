package stag.output

import chisel3._
import stag.common.{AdderTreeOperation, Mac, MultiplierOperation}

class VectorProcessingElement[InputTypeA <: Data, InputTypeB <: Data, MultOutputType <: Data, AdderOutputType <: Data, OutputTypeC <: Data](
  peMultiplierCount: Int,
  inputTypeA: InputTypeA,
  inputTypeB: InputTypeB,
  outputTypeC: OutputTypeC
)( implicit
  evMult: MultiplierOperation[InputTypeA, InputTypeB, MultOutputType],
  evAdd: AdderTreeOperation[MultOutputType, AdderOutputType],
  evPe: ProcessingElementOperation[InputTypeA, InputTypeB, AdderOutputType, OutputTypeC]
) extends Module {

  val mac= Module(new Mac(peMultiplierCount, inputTypeA, inputTypeB))
  val outputRegister = RegInit(evPe.zero)

  val io = IO(new Bundle {
    val inputA = Input(Vec(peMultiplierCount, inputTypeA))
    val inputB = Input(Vec(peMultiplierCount, inputTypeB))
    val partialSumReset: Bool = Input(Bool())
    val output = Output(outputTypeC)
  })

  //Wiring input
  mac.io.inputA := io.inputA
  mac.io.inputB := io.inputB

  //Wiring control and output
  outputRegister := evPe.add(mac.io.output, Mux(io.partialSumReset, evPe.zero, outputRegister))
  io.output := outputRegister

}
