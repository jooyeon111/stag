package stag.weight

import chisel3._
import stag.common.{Arithmetic, Mac, PortConfig}

class VectorProcessingElement[T <: Data](
  peMultiplierCount: Int,
  flagInputC: Boolean,
  portConfig: PortConfig[T],
)( implicit ev: Arithmetic[T] ) extends Module {

  val mac = Module(new Mac(peMultiplierCount, portConfig.inputTypeA, portConfig.inputTypeB, portConfig.multiplierOutputType, portConfig.adderTreeOutputTypeType))

  val io =  IO(new Bundle {

    //Input
    val inputA = Input(Vec(peMultiplierCount, portConfig.inputTypeA))
    val inputB = Input(Vec(peMultiplierCount, portConfig.inputTypeB))
    val inputC = if(flagInputC) Some(Input(portConfig.outputTypeC)) else None

    //Control
    val propagateB: Bool = Input(Bool())

    //Output
    val outputB = Output(Vec(peMultiplierCount, portConfig.inputTypeB))
    val outputC = Output(portConfig.outputTypeC)

  })

  io.outputB := RegNext(Mux(io.propagateB, io.inputB, io.outputB), VecInit.fill(peMultiplierCount)(ev.zero(portConfig.inputTypeB.getWidth)))

  mac.io.inputA := io.inputA
  mac.io.inputB := io.inputB

  if(flagInputC)
    io.outputC := RegNext(ev.add(mac.io.output, io.inputC.get), ev.zero(portConfig.outputTypeC.getWidth))
  else
    io.outputC := RegNext(mac.io.output, ev.zero(portConfig.outputTypeC.getWidth))


}
