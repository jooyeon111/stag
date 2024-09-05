package stag.weight

import chisel3._
import stag.common.{PortConfig, PreProcessor, SystolicTensorArrayConfig}

class DimensionAlignedSystolicTensorArray(val groupPeRow: Int, val groupPeCol : Int, val vectorPeRow : Int, val vectorPeCol : Int, val numPeMultiplier : Int, portConfig: PortConfig) extends Module {

  def this(arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig) =
    this(arrayConfig.groupPeRow, arrayConfig.groupPeCol, arrayConfig.vectorPeRow, arrayConfig.vectorPeCol, arrayConfig.numPeMultiplier, portConfig)

  val numInputA: Int = groupPeRow * vectorPeRow * numPeMultiplier
  val numInputB: Int = groupPeCol * vectorPeCol * numPeMultiplier
  val numPropagateB: Int = groupPeRow * vectorPeRow
  val numOutput : Int = groupPeCol * vectorPeCol

  val preProcessorInputA = Module (new PreProcessor(groupPeRow, vectorPeRow, numPeMultiplier, skewFlag = true, portConfig.bitWidthA))
  val preProcessorInputB = Module (new PreProcessor(groupPeRow, vectorPeRow, numPeMultiplier, skewFlag = false, portConfig.bitWidthA))
  val systolicTensorArray = Module (new SystolicTensorArray(groupPeRow, groupPeCol, vectorPeRow, vectorPeCol, numPeMultiplier, portConfig, generateRtl = false))
  val postProcessor = Module (new PostProcessor(groupPeCol, vectorPeCol, portConfig.bitWidthC))

  // register in front of control signals
  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numInputA, SInt(8.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB, SInt(8.W)))
    val propagateB : Vec[Bool] = Input(Vec(numPropagateB, Bool()))
    val outputC: Vec[SInt] = Output(Vec(numOutput, SInt(32.W)))
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
