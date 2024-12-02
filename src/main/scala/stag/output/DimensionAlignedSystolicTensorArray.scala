package stag.output

import chisel3._
import stag.common.{Arithmetic, PortConfig, PreProcessor, PreProcessorType, SystolicTensorArrayConfig, VerilogNaming}

//Pod = Pre Processing Unit +  Systolic Tensor Array + Post Processing Unit
class DimensionAlignedSystolicTensorArray[T <: Data](
  groupPeRow: Int,
  groupPeCol : Int,
  vectorPeRow : Int,
  vectorPeCol : Int,
  numPeMultiplier : Int,
  dedicatedName: String,
  portConfig: PortConfig[T],
)(implicit ev: Arithmetic[T]) extends Module with VerilogNaming
 {
  def this(arrayConfig: SystolicTensorArrayConfig, dedicatedName: String, portConfig: PortConfig[T])(implicit ev: Arithmetic[T]) =
    this(arrayConfig.groupPeRow, arrayConfig.groupPeCol, arrayConfig.vectorPeRow, arrayConfig.vectorPeCol, arrayConfig.numPeMultiplier, dedicatedName, portConfig)

  override def desiredName: String = dedicatedName

  val numInputA: Int = groupPeRow * vectorPeRow * numPeMultiplier
  val numInputB: Int = groupPeCol * vectorPeCol * numPeMultiplier
  val numPartialSumReset = groupPeRow + groupPeCol - 1
  val numPropagateOutput: Int = groupPeCol - 1
  val numOutput: Int = (groupPeCol + groupPeRow - 1)* vectorPeRow * vectorPeCol
  val outputTypeC = portConfig.getStaOutputTypeC

  val preProcessorInputA = Module (new PreProcessor(
    groupPeRow,
    vectorPeRow,
    numPeMultiplier,
    skewFlag = true,
    PreProcessorType.A,
    portConfig.inputTypeA
  ))

  val preProcessorInputB = Module (new PreProcessor(
    groupPeCol,
    vectorPeCol,
    numPeMultiplier,
    skewFlag = true,
    PreProcessorType.B,
    portConfig.inputTypeB
  ))

  val systolicTensorArray = Module (new SystolicTensorArray(
    groupPeRow,
    groupPeCol,
    vectorPeRow,
    vectorPeCol,
    numPeMultiplier,
    portConfig))

  val postProcessor = Module (new DeskewBuffer(
    groupPeRow,
    groupPeCol,
    vectorPeRow,
    vectorPeCol,
    outputTypeC
  ))

  val io = IO(new Bundle {
    val inputA = Input(Vec(numInputA, portConfig.inputTypeA))
    val inputB = Input(Vec(numInputB, portConfig.inputTypeB))
    val propagateOutput =  Input(Vec(numPropagateOutput, Bool()))
    val partialSumReset =  Input(Vec(numPartialSumReset, Bool()))
    val outputC = Output(Vec(numOutput, outputTypeC))
  })

  //Wiring Input
  preProcessorInputA.io.input := io.inputA
  preProcessorInputB.io.input := io.inputB
  systolicTensorArray.io.inputA := preProcessorInputA.io.output
  systolicTensorArray.io.inputB := preProcessorInputB.io.output
  postProcessor.io.input := systolicTensorArray.io.outputC

  //Wiring propagate signal
  systolicTensorArray.io.partialSumReset := RegNext(io.partialSumReset, VecInit.fill(numPartialSumReset)(false.B))

  //Wiring partial sum signals
  systolicTensorArray.io.propagateOutput := RegNext( io.propagateOutput, VecInit.fill(numPropagateOutput)(false.B))

  //Wiring Output
  io.outputC := postProcessor.io.output

}
