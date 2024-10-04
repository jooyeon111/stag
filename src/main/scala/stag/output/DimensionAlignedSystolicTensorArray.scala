package stag.output

import chisel3._
import stag.common.{ PortConfig, PreProcessor, SystolicTensorArrayConfig, Arithmetic}

//Pod = Pre Processing Unit +  Systolic Tensor Array + Post Processing Unit
class DimensionAlignedSystolicTensorArray[T <: Data](
  groupPeRow: Int,
  groupPeCol : Int,
  vectorPeRow : Int,
  vectorPeCol : Int,
  numPeMultiplier : Int,
  portConfig: PortConfig[T],
  outputTypeC: T
)(implicit ev: Arithmetic[T]) extends Module {

  def this(arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig[T], outputPortType: T)(implicit ev: Arithmetic[T]) =
    this(arrayConfig.groupPeRow, arrayConfig.groupPeCol, arrayConfig.vectorPeRow, arrayConfig.vectorPeCol, arrayConfig.numPeMultiplier, portConfig, outputPortType)

  val numInputA: Int = groupPeRow * vectorPeRow * numPeMultiplier
  val numInputB: Int = groupPeCol * vectorPeCol * numPeMultiplier
  val numOutput: Int = (groupPeCol + groupPeRow - 1)* vectorPeRow * vectorPeCol

  val preProcessorInputA = Module (new PreProcessor(groupPeRow, vectorPeRow, numPeMultiplier, skewFlag = true, portConfig.inputTypeA))
  val preProcessorInputB = Module (new PreProcessor(groupPeCol, vectorPeCol, numPeMultiplier, skewFlag = true, portConfig.inputTypeB))
  val systolicTensorArray = Module (new SystolicTensorArray(groupPeRow, groupPeCol, vectorPeRow, vectorPeCol, numPeMultiplier, portConfig, outputTypeC, generateRtl = false))
  val postProcessor = Module (new DeskewBuffer(groupPeRow, groupPeCol, vectorPeRow, vectorPeCol, outputTypeC))

  val io = IO(new Bundle {
    val inputA = Input(Vec(numInputA, portConfig.inputTypeA))
    val inputB = Input(Vec(numInputB, portConfig.inputTypeB))
    val propagateOutput =  Input(Vec(groupPeRow - 1, Vec(groupPeCol - 1, Bool())))
    val partialSumReset =  Input(Vec(groupPeRow, Vec(groupPeCol, Bool())))
    val outputC = Output(Vec(numOutput, outputTypeC))
  })

  //Wiring Input
  preProcessorInputA.io.input := io.inputA
  preProcessorInputB.io.input := io.inputB
  systolicTensorArray.io.inputA := preProcessorInputA.io.output
  systolicTensorArray.io.inputB := preProcessorInputB.io.output
  postProcessor.io.input := systolicTensorArray.io.outputC

  //Wiring propagate signal
  systolicTensorArray.io.propagateOutput := RegNext( io.propagateOutput, VecInit.fill(groupPeRow - 1, groupPeCol -1)(false.B) )

  //Wiring partial sum signals
  systolicTensorArray.io.partialSumReset := RegNext( io.partialSumReset, VecInit.fill(groupPeRow, groupPeCol)(false.B)  )

  //Wiring Output
  io.outputC := systolicTensorArray.io.outputC

}
