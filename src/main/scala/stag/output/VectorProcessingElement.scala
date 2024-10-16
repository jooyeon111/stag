package stag.output

import chisel3._
import stag.common.{ Mac, PortConfig, Arithmetic}

class VectorProcessingElement[T <: Data](
  numPeMultiplier: Int,
  portConfig: PortConfig[T],
)( implicit ev: Arithmetic[T] ) extends Module {

  val mac= Module(new Mac(numPeMultiplier, portConfig.inputTypeA, portConfig.inputTypeB, portConfig.multiplierOutputType, portConfig.adderTreeOutputTypeType))
  val outputTypeC = portConfig.getStaOutputTypeC
  val outputRegister = RegInit(ev.zero(outputTypeC.getWidth))

  val io = IO(new Bundle {
    val inputA = Input(Vec(numPeMultiplier, portConfig.inputTypeA))
    val inputB = Input(Vec(numPeMultiplier, portConfig.inputTypeB))
    val partialSumReset: Bool = Input(Bool())
    val output = Output(outputTypeC)
  })

  mac.io.inputA := io.inputA
  mac.io.inputB := io.inputB
  outputRegister := ev.add(mac.io.output, Mux(io.partialSumReset, ev.zero(outputTypeC.getWidth), outputRegister))
  io.output := outputRegister

}
