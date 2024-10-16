package stag.input

import chisel3._
import stag.common.Mac
import stag.common.Arithmetic
import stag.common.PortConfig

class VectorProcessingElement[T <: Data](
  groupPeColIndex: Int,
  vectorPeColIndex: Int,
  vectorPeCol: Int,
  numPeMultiplier: Int,
  flagInputC: Boolean,
  portConfig: PortConfig[T]
)( implicit ev: Arithmetic[T] ) extends Module {

  val outputTypeC = if (portConfig.enableUserBitWidth)
    portConfig.getStaOutputTypeC
  else
    portConfig.calculateOutputTypeC(portConfig.adderTreeOutputTypeType.getWidth + vectorPeColIndex + (groupPeColIndex * vectorPeCol))

  val io =  IO(new Bundle {

    //Input
    val inputA = Input(Vec(numPeMultiplier, portConfig.inputTypeA))
    val inputB = Input(Vec(numPeMultiplier, portConfig.inputTypeB))
    val inputC = if(flagInputC) Some(Input(outputTypeC)) else None

    //Control
    val propagateA: Bool = Input(Bool())

    //Output
    val outputA = Output(Vec(numPeMultiplier, portConfig.inputTypeA))
    val outputC = Output(portConfig.inputTypeB)

  })

  val mac = Module(new Mac(numPeMultiplier, portConfig.inputTypeA, portConfig.inputTypeB, portConfig.multiplierOutputType, portConfig.adderTreeOutputTypeType))

  io.outputA := RegNext(Mux(io.propagateA, io.inputA, io.outputA), VecInit.fill(numPeMultiplier)(ev.zero(portConfig.inputTypeA.getWidth)))

  mac.io.inputA := io.inputA
  mac.io.inputB := io.inputB

  if(flagInputC)
    io.outputC := RegNext(ev.add(mac.io.output, io.inputC.get), ev.zero(outputTypeC.getWidth))
  else
    io.outputC := RegNext(mac.io.output, ev.zero(outputTypeC.getWidth))

}
