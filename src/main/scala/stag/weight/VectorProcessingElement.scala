package stag.weight

import chisel3._
import chisel3.util.log2Ceil
import stag.common.{Arithmetic, Mac, PortConfig, ParallelMultiplier}

class VectorProcessingElement[T <: Data](
  groupPeRowIndex: Int,
  vectorPeRowIndex: Int,
  vectorPeRow: Int,
  numPeMultiplier: Int,
  withOutputB: Boolean,
  withInputC: Boolean,
  portConfig: PortConfig[T],
)( implicit ev: Arithmetic[T] ) extends Module {

  override def desiredName: String = if(numPeMultiplier == 1) "ProcessingElement" else "VectorProcessingElement"

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

  val registerB = RegInit(VecInit.fill(numPeMultiplier)(ev.zero(portConfig.inputTypeB.getWidth)))
  val nextRegisterB = WireDefault(registerB)

  when(io.propagateB) {
    nextRegisterB := io.inputB
  }
  registerB := nextRegisterB

  if(withOutputB)
    io.outputB.get := registerB


  val multiplyResult = if (numPeMultiplier == 1){

    val multiplier = Module(new ParallelMultiplier(
      numPeMultiplier = numPeMultiplier,
      portConfig.inputTypeA,
      portConfig.inputTypeB,
      portConfig.multiplierOutputType
    ))

    multiplier.io.inputA(0) := io.inputA(0)
    multiplier.io.inputB(0) := registerB(0)
    multiplier.io.output(0)

  } else {

    val mac = Module(new Mac(
      numPeMultiplier = numPeMultiplier,
      portConfig.inputTypeA,
      portConfig.inputTypeB,
      portConfig.multiplierOutputType,
      portConfig.adderTreeOutputTypeType
    ))

    mac.io.inputA := io.inputA
    mac.io.inputB := registerB
    mac.io.output

  }

  if(withInputC)
    io.outputC := RegNext(ev.add(multiplyResult, io.inputC.get), ev.zero(outputTypeC.getWidth))
  else
    io.outputC := RegNext(multiplyResult, ev.zero(outputTypeC.getWidth))

}
