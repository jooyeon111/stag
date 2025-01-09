package stag.output

import chisel3._
import stag.common.{Mac, PortConfig, Arithmetic, ParallelMultiplier, VerilogNaming}

class VectorProcessingElement[T <: Data](
  numPeMultiplier: Int,
  withOutputA: Boolean,
  withOutputB: Boolean,
  withInputC: Boolean,
  portConfig: PortConfig[T],
)( implicit ev: Arithmetic[T] ) extends Module with VerilogNaming with ProcessingElementIo[T] {

  override def desiredName: String = camelToSnake( if(numPeMultiplier == 1) "ProcessingElement" else "VectorProcessingElement")

  val outputTypeC = portConfig.getStaOutputTypeC
  val partialSumRegister = RegInit(ev.zero(outputTypeC.getWidth))

  override type OutputType = T

  override val io = IO(new Bundle {
    val inputA = Input(Vec(numPeMultiplier, portConfig.inputTypeA))
    val inputB = Input(Vec(numPeMultiplier, portConfig.inputTypeB))
    val inputC = if(withInputC) Some(Input(outputTypeC)) else None

    val partialSumReset = Input(Bool())
    val propagateOutput = if(withInputC) Some(Input(Bool())) else None

    val outputA = if(withOutputA) Some(Output(Vec(numPeMultiplier, portConfig.inputTypeA))) else None
    val outputB = if(withOutputB) Some(Output(Vec(numPeMultiplier, portConfig.inputTypeB))) else None
    val outputC = Output(outputTypeC)
  })

  if(withOutputA){
    val registerA = RegInit(VecInit.fill(numPeMultiplier)(ev.zero(portConfig.inputTypeA.getWidth)))
    registerA := io.inputA
    io.outputA.get := registerA
  }

  if(withOutputB){
    val registerB = RegInit(VecInit.fill(numPeMultiplier)(ev.zero(portConfig.inputTypeB.getWidth)))
    io.outputB.get := registerB
  }

  val multiplyResult = if (numPeMultiplier == 1){

    val multiplier = Module(new ParallelMultiplier(
      numPeMultiplier = numPeMultiplier,
      portConfig.inputTypeA,
      portConfig.inputTypeB,
      portConfig.multiplierOutputType
    ))

    multiplier.io.inputA(0) := io.inputA(0)
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
    mac.io.inputA := io.inputA
    mac.io.inputB := io.inputB
    mac.io.output

  }

  val partialSum = WireDefault(partialSumRegister)

  when(io.partialSumReset) {
    partialSum := ev.zero(outputTypeC.getWidth)
  }

  partialSumRegister := ev.add(multiplyResult, partialSum)

  if(withInputC){
    val outputRegister = RegInit(ev.zero(outputTypeC.getWidth))
    outputRegister := Mux(io.propagateOutput.get, io.inputC.get, partialSumRegister)
    io.outputC := outputRegister
  } else {
    io.outputC := partialSumRegister
  }

}
