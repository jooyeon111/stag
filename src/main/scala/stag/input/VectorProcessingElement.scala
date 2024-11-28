package stag.input

import chisel3._
import chisel3.util.log2Ceil
import stag.common.{Arithmetic, Mac, ParallelMultiplier, PortConfig}

class VectorProcessingElement[T <: Data](
  groupPeColIndex: Int,
  vectorPeColIndex: Int,
  vectorPeCol: Int,
  numPeMultiplier: Int,
  withOutputA: Boolean,
  withInputC: Boolean,
  portConfig: PortConfig[T]
)( implicit ev: Arithmetic[T] ) extends Module {

  override def desiredName: String = if(numPeMultiplier == 1) "ProcessingElement" else "VectorProcessingElement"

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


  val registerA = RegInit(VecInit.fill(numPeMultiplier)(ev.zero(portConfig.inputTypeA.getWidth)))
  val nextRegisterA = WireDefault(registerA)

  when(io.propagateA) {
    nextRegisterA := io.inputA
  }
  registerA := nextRegisterA

  if(withOutputA)
    io.outputA.get := registerA


  val multiplyResult = if (numPeMultiplier == 1) {

    assert(portConfig.adderTreeOutputTypeType.getWidth == portConfig.multiplierOutputType.getWidth,
    "Port output bit width calculation is wrong")

    val multiplier = Module(new ParallelMultiplier(
      numPeMultiplier = numPeMultiplier,
      portConfig.inputTypeA,
      portConfig.inputTypeB,
      portConfig.multiplierOutputType
    ))

    multiplier.io.inputA(0) := registerA(0)
    multiplier.io.inputB(0) := io.inputB(0)
    multiplier.io.output(0)

  } else {
    val mac = Module(new Mac(
      numPeMultiplier = numPeMultiplier,
      portConfig.inputTypeA,
      portConfig.inputTypeB,
      portConfig.multiplierOutputType,
      portConfig.adderTreeOutputTypeType
    ))

    mac.io.inputA := registerA
    mac.io.inputB := io.inputB
    mac.io.output

  }

  if(withInputC)
    io.outputC := RegNext(ev.add(multiplyResult, io.inputC.get), ev.zero(outputTypeC.getWidth))
  else
    io.outputC := RegNext(multiplyResult, ev.zero(outputTypeC.getWidth))

}
