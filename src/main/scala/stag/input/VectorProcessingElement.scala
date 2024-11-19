package stag.input

import chisel3._
import chisel3.util.log2Ceil
import stag.common.{Mac, Arithmetic, PortConfig}

class VectorProcessingElement[T <: Data](
  groupPeColIndex: Int,
  vectorPeColIndex: Int,
  vectorPeCol: Int,
  numPeMultiplier: Int,
  withOutputA: Boolean,
  withInputC: Boolean,
  portConfig: PortConfig[T]
)( implicit ev: Arithmetic[T] ) extends Module {

  val outputTypeC = if (portConfig.enableUserBitWidth)
    portConfig.getStaOutputTypeC
  else
    portConfig.calculateOutputTypeC(
      portConfig.adderTreeOutputTypeType.getWidth + log2Ceil( (vectorPeColIndex + 1) + groupPeColIndex * vectorPeCol)
    )

  val io =  IO(new Bundle {

    //Input
    val inputA = Input(Vec(numPeMultiplier, portConfig.inputTypeA))
    val inputB = Input(Vec(numPeMultiplier, portConfig.inputTypeB))
    val inputC = if(withInputC) Some(Input(outputTypeC)) else None

    //Control
    val propagateA: Bool = Input(Bool())

    //Output
    val outputA = if(withOutputA) Some ( Output(Vec(numPeMultiplier, portConfig.inputTypeA)) ) else None
    val outputC = Output(outputTypeC)

  })

  val mac = Module(new Mac(
    numPeMultiplier,
    portConfig.inputTypeA,
    portConfig.inputTypeB,
    portConfig.multiplierOutputType,
    portConfig.adderTreeOutputTypeType
  ))
  val registerA = RegInit(VecInit.fill(numPeMultiplier)(ev.zero(portConfig.inputTypeA.getWidth)))
  val nextRegisterA = WireDefault(registerA)

  when(io.propagateA) {
    nextRegisterA := io.inputA
  }
  registerA := nextRegisterA

  if(withOutputA)
    io.outputA.get := registerA

  mac.io.inputA := registerA
  mac.io.inputB := io.inputB

  if(withInputC)
    io.outputC := RegNext(ev.add(mac.io.output, io.inputC.get), ev.zero(outputTypeC.getWidth))
  else
    io.outputC := RegNext(mac.io.output, ev.zero(outputTypeC.getWidth))

}
