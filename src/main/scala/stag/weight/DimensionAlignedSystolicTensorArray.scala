package stag.weight

import chisel3._
import stag.common.{Arithmetic, PortConfig, PreProcessor, PreProcessorType, SystolicTensorArrayConfig}

class DimensionAlignedSystolicTensorArray[T <: Data](
  groupPeRow: Int,
  groupPeCol : Int,
  vectorPeRow : Int,
  vectorPeCol : Int,
  numPeMultiplier : Int,
  dedicatedName: String,
  portConfig: PortConfig[T]
)(implicit ev: Arithmetic[T]) extends Module {

  def this(arrayConfig: SystolicTensorArrayConfig, dedicatedName: String, portConfig: PortConfig[T])(implicit ev: Arithmetic[T]) =
    this(
      arrayConfig.groupPeRow,
      arrayConfig.groupPeCol,
      arrayConfig.vectorPeRow,
      arrayConfig.vectorPeCol,
      arrayConfig.numPeMultiplier,
      dedicatedName,
      portConfig
    )

  override def desiredName: String = dedicatedName

  val numInputA: Int = groupPeRow * vectorPeRow * numPeMultiplier
  val numInputB: Int = groupPeCol * vectorPeCol * numPeMultiplier
  val numPropagateB: Int = groupPeRow * vectorPeRow
  val numOutput : Int = groupPeCol * vectorPeCol

  val preProcessorInputA = Module (new PreProcessor(
    groupPeRow,
    vectorPeRow,
    numPeMultiplier,
    skewFlag = true,
    PreProcessorType.A,
    portConfig.inputTypeA,
  ))
  val preProcessorInputB = Module (new PreProcessor(
    groupPeCol,
    vectorPeCol,
    numPeMultiplier,
    skewFlag = false,
    PreProcessorType.B,
    portConfig.inputTypeB
  ))
  val systolicTensorArray = Module (new SystolicTensorArray(
    groupPeRow,
    groupPeCol,
    vectorPeRow,
    vectorPeCol,
    numPeMultiplier,
    portConfig,
    generateRtl = false
  ))
  val postProcessor = Module (new PostProcessor(
    groupPeCol,
    vectorPeCol,
    systolicTensorArray.outputTypeC
  ))

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
  io.outputC := postProcessor.io.output

}
