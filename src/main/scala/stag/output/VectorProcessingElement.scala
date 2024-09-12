package stag.output

import chisel3._
import stag.common.{ Mac, PortConfig, Arithmetic}

class VectorProcessingElement[T <: Data](
  peMultiplierCount: Int,
  portConfig: PortConfig[T],
)( implicit ev: Arithmetic[T] ) extends Module {

  val mac= Module(new Mac(peMultiplierCount, portConfig.inputTypeA, portConfig.inputTypeB, portConfig.multiplierOutputType, portConfig.adderTreeOutputTypeType))
  val outputRegister = RegInit(ev.zero(portConfig.outputTypeC.getWidth))

  val io = IO(new Bundle {
    val inputA = Input(Vec(peMultiplierCount, portConfig.inputTypeA))
    val inputB = Input(Vec(peMultiplierCount, portConfig.inputTypeB))
    val partialSumReset: Bool = Input(Bool())
    val output = Output(portConfig.outputTypeC)
  })

  mac.io.inputA := io.inputA
  mac.io.inputB := io.inputB
  outputRegister := ev.add(mac.io.output, Mux(io.partialSumReset, ev.zero(portConfig.outputTypeC.getWidth), outputRegister))
  io.output := outputRegister

}
