package stag.weight

import chisel3._
import stag.common.{PortConfig, PreProcessor, SystolicTensorArrayConfig, Arithmetic}

class DimensionAlignedSystolicTensorArray[T <: Data](
  groupPeRow: Int,
  groupPeCol : Int,
  vectorPeRow : Int,
  vectorPeCol : Int,
  numPeMultiplier : Int,
  portConfig: PortConfig[T]
)(implicit ev: Arithmetic[T]) extends Module {

  def this(arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig[T])(implicit ev: Arithmetic[T]) =
    this(arrayConfig.groupPeRow, arrayConfig.groupPeCol, arrayConfig.vectorPeRow, arrayConfig.vectorPeCol, arrayConfig.numPeMultiplier, portConfig)

  //TODO fix below code
  val numInputA: Int = groupPeRow * vectorPeRow * numPeMultiplier
  val numInputB: Int = groupPeCol * vectorPeCol * numPeMultiplier
  val numPropagateB: Int = groupPeRow * vectorPeRow
  val numOutput : Int = groupPeCol * vectorPeCol
//  val outputTypeC = portConfig.calculateOutputTypeC(
//    portConfig.adderTreeOutputTypeType.getWidth
//      + (groupPeRow * vectorPeRow)
//  )

  val preProcessorInputA = Module (new PreProcessor(groupPeRow, vectorPeRow, numPeMultiplier, skewFlag = true, portConfig.inputTypeA))
  val preProcessorInputB = Module (new PreProcessor(groupPeCol, vectorPeCol, numPeMultiplier, skewFlag = false, portConfig.inputTypeB))
  val systolicTensorArray = Module (new SystolicTensorArray(groupPeRow, groupPeCol, vectorPeRow, vectorPeCol, numPeMultiplier, portConfig, generateRtl = false))
  val postProcessor = Module (new PostProcessor(groupPeCol, vectorPeCol, systolicTensorArray.outputTypeC))

  // register in front of control signals
  val io = IO(new Bundle {
    val inputA = Input(Vec(numInputA, portConfig.inputTypeA))
    val inputB = Input(Vec(numInputB, portConfig.inputTypeB))
    val propagateB = Input(Vec(numPropagateB, Bool()))
    val outputC = Output(Vec(numOutput, systolicTensorArray.outputTypeC))
  })

  //Wiring Input
  preProcessorInputA.io.input := io.inputA
  preProcessorInputB.io.input := io.inputB
  systolicTensorArray.io.inputA := preProcessorInputA.io.output
  systolicTensorArray.io.inputB := preProcessorInputB.io.output
  postProcessor.io.input := systolicTensorArray.io.outputC

  //Wiring Control
  systolicTensorArray.io.propagateB := RegNext(io.propagateB, VecInit.fill(numPropagateB)(false.B))

  //Wiring Output
  io.outputC := systolicTensorArray.io.outputC

}
