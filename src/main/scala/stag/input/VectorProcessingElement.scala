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
  withInputC: Boolean,
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
    val inputC = if(withInputC) Some(Input(outputTypeC)) else None

    //Control
    val propagateA: Bool = Input(Bool())

    //Output
    val outputA = Output(Vec(numPeMultiplier, portConfig.inputTypeA))
    val outputC = Output(portConfig.inputTypeB)

  })

  val mac = Module(new Mac(numPeMultiplier, portConfig.inputTypeA, portConfig.inputTypeB, portConfig.multiplierOutputType, portConfig.adderTreeOutputTypeType))
  val registerA = RegInit(VecInit.fill(numPeMultiplier)(ev.zero(portConfig.inputTypeA.getWidth)))

  registerA := Mux(io.propagateA, io.inputA, io.outputA)
  io.outputA := registerA

  mac.io.inputA := registerA
  mac.io.inputB := io.inputB

  if(withInputC)
    io.outputC := RegNext(ev.add(mac.io.output, io.inputC.get), ev.zero(outputTypeC.getWidth))
  else
    io.outputC := RegNext(mac.io.output, ev.zero(outputTypeC.getWidth))

}
