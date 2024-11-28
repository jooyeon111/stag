package stag.output

import chisel3._
import stag.common.{Mac, PortConfig, Arithmetic, ParallelMultiplier, VerilogNaming}

class VectorProcessingElement[T <: Data](
  numPeMultiplier: Int,
  portConfig: PortConfig[T],
)( implicit ev: Arithmetic[T] ) extends Module with VerilogNaming{

  override def desiredName: String = camelToSnake( if(numPeMultiplier == 1) "ProcessingElement" else "VectorProcessingElement")

  val outputTypeC = portConfig.getStaOutputTypeC
  val outputRegister = RegInit(ev.zero(outputTypeC.getWidth))

  val io = IO(new Bundle {
    val inputA = Input(Vec(numPeMultiplier, portConfig.inputTypeA))
    val inputB = Input(Vec(numPeMultiplier, portConfig.inputTypeB))
    val partialSumReset = Input(Bool())
    val output = Output(outputTypeC)
  })

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

  val partialSum = WireDefault(outputRegister)

  when(io.partialSumReset) {
    partialSum := ev.zero(outputTypeC.getWidth)
  }

  outputRegister := ev.add(multiplyResult, partialSum)
  io.output := outputRegister

}
