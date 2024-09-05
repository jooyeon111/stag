package stag.output

import chisel3._
import stag.common.{PreProcessor, SystolicTensorArrayConfig, PortConfig}

//Pod = Pre Processing Unit +  Systolic Tensor Array + Post Processing Unit
class DimensionAlignedSystolicTensorArray(val groupPeRow: Int, val groupPeCol : Int, val vectorPeRow : Int, val vectorPeCol : Int, val numPeMultiplier : Int, portConfig: PortConfig) extends Module {

  def this(arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig) =
    this(arrayConfig.groupPeRow, arrayConfig.groupPeCol, arrayConfig.vectorPeRow, arrayConfig.vectorPeCol, arrayConfig.numPeMultiplier, portConfig)

  val numInputA: Int = groupPeRow * vectorPeRow * numPeMultiplier
  val numInputB: Int = groupPeCol * vectorPeCol * numPeMultiplier
  val numOutput: Int = (groupPeCol + groupPeRow - 1)* vectorPeRow * vectorPeCol

  val preProcessorInputA = Module (new PreProcessor(groupPeRow, vectorPeRow, numPeMultiplier, skewFlag = true, portConfig.bitWidthA))
  val preProcessorInputB = Module (new PreProcessor(groupPeCol, vectorPeCol, numPeMultiplier, skewFlag = true, portConfig.bitWidthB))
  val systolicTensorArray = Module (new SystolicTensorArray(groupPeRow, groupPeCol, vectorPeRow, vectorPeCol, numPeMultiplier, portConfig, generateRtl = false))
  val postProcessor = Module (new PostProcessor(groupPeRow, groupPeCol, vectorPeRow, vectorPeCol, portConfig.bitWidthC))

  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numInputA,SInt(portConfig.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB,SInt(portConfig.bitWidthB.W)))
    val propagateOutput: Vec[Vec[Bool]] =  Input(Vec(groupPeRow - 1, Vec(groupPeCol - 1, Bool())))
    val partialSumReset: Vec[Vec[Bool]] =  Input(Vec(groupPeRow, Vec(groupPeCol, Bool())))
    val outputC: Vec[SInt] = Output(Vec(numOutput,SInt(portConfig.bitWidthC.W)))
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
