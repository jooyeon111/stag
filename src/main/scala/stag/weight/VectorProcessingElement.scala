package stag.weight

import chisel3._
import chisel3.util.log2Ceil
import stag.common.{Arithmetic, Mac, PortConfig}

class VectorProcessingElement[T <: Data](
  groupPeRowIndex: Int,
  vectorPeRowIndex: Int,
  vectorPeRow: Int,
  numPeMultiplier: Int,
  withOutputB: Boolean,
  withInputC: Boolean,
  portConfig: PortConfig[T],
)( implicit ev: Arithmetic[T] ) extends Module {

  val outputTypeC = if(portConfig.enableUserBitWidth)
    portConfig.getStaOutputTypeC
  else
    portConfig.calculateOutputTypeC(
      portConfig.adderTreeOutputTypeType.getWidth + log2Ceil( (vectorPeRowIndex + 1) + groupPeRowIndex * vectorPeRow)
    )

  val io =  IO(new Bundle {

    //Input
    val inputA = Input(Vec(numPeMultiplier, portConfig.inputTypeA))
    val inputB = Input(Vec(numPeMultiplier, portConfig.inputTypeB))
    val inputC = if(withInputC) Some(Input(outputTypeC)) else None

    //Control
    val propagateB: Bool = Input(Bool())

    //Output
    val outputB = if(withOutputB) Some (Output(Vec(numPeMultiplier, portConfig.inputTypeB))) else None
    val outputC = Output(outputTypeC)

  })

  val mac = Module(new Mac(
    numPeMultiplier,
    portConfig.inputTypeA,
    portConfig.inputTypeB,
    portConfig.multiplierOutputType,
    portConfig.adderTreeOutputTypeType
  ))
  val registerB = RegInit(VecInit.fill(numPeMultiplier)(ev.zero(portConfig.inputTypeB.getWidth)))
  val nextRegisterB = WireDefault(registerB)

  when(io.propagateB) {
    nextRegisterB := io.inputB
  }
  registerB := nextRegisterB

  if(withOutputB)
    io.outputB.get := registerB

  mac.io.inputA := io.inputA
  mac.io.inputB := registerB

  if(withInputC)
    io.outputC := RegNext(ev.add(mac.io.output, io.inputC.get), ev.zero(outputTypeC.getWidth))
  else
    io.outputC := RegNext(mac.io.output, ev.zero(outputTypeC.getWidth))

}
