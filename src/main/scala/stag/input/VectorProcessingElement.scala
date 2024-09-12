package stag.input

import chisel3._
import stag.common.Mac
import stag.common.Arithmetic
import stag.common.PortConfig

class VectorProcessingElement[T <: Data](
  peMultiplierCount: Int,
  flagInputC: Boolean,
  portConfig: PortConfig[T]
)( implicit ev: Arithmetic[T] ) extends Module {

  //TODO fix it later just temporal code to prevent an error
  val mac = Module(new Mac(peMultiplierCount, portConfig.inputTypeA, portConfig.inputTypeB, portConfig.multiplierOutputType, portConfig.adderTreeOutputTypeType))

  val io =  IO(new Bundle {

    //Input
    val inputA = Input(Vec(peMultiplierCount, portConfig.inputTypeA))
    val inputB = Input(Vec(peMultiplierCount, portConfig.inputTypeB))
    val inputC = if(flagInputC) Some(Input(portConfig.outputTypeC)) else None

    //Control
    val propagateA: Bool = Input(Bool())

    //Output
    val outputA = Output(Vec(peMultiplierCount, portConfig.inputTypeA))
    val outputC = Output(portConfig.inputTypeB)

  })

  io.outputA := RegNext(Mux(io.propagateA, io.inputA, io.outputA), VecInit.fill(peMultiplierCount)(ev.zero(portConfig.inputTypeA.getWidth)))

  mac.io.inputA := io.inputA
  mac.io.inputB := io.inputB

  if(flagInputC)
    io.outputC := RegNext(ev.add(mac.io.output, io.inputC.get), ev.zero(portConfig.outputTypeC.getWidth))
  else
    io.outputC := RegNext(mac.io.output, ev.zero(portConfig.outputTypeC.getWidth))


}
