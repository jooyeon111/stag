package stag.output

import chisel3._
import stag.common.{PreProcessor, SystolicTensorArrayConfig, PortConfig}

//Pod = Pre Processing Unit +  Systolic Tensor Array + Post Processing Unit
class SystolicTensorArrayPod(val arrayRow: Int, val arrayCol : Int, val blockRow : Int, val blockCol : Int, val numPeMultiplier : Int, portConfig: PortConfig) extends Module {

  def this(arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig) =
    this(arrayConfig.arrayRow, arrayConfig.arrayCol, arrayConfig.blockRow, arrayConfig.blockCol, arrayConfig.numPeMultiplier, portConfig)

  val numInputA: Int = arrayRow * blockRow * numPeMultiplier
  val numInputB: Int = arrayCol * blockCol * numPeMultiplier
  val numOutput: Int = (arrayCol + arrayRow - 1)* blockRow * blockCol

  val preProcessorInputA = Module (new PreProcessor(arrayRow, blockRow, numPeMultiplier, skewFlag = true, portConfig.bitWidthA))
  val preProcessorInputB = Module (new PreProcessor(arrayCol, blockCol, numPeMultiplier, skewFlag = true, portConfig.bitWidthB))
  val systolicTensorArray = Module (new SystolicTensorArray(arrayRow, arrayCol, blockRow, blockCol, numPeMultiplier, portConfig, generateRtl = false))
  val postProcessor = Module (new PostProcessor(arrayRow, arrayCol, blockRow, blockCol, portConfig.bitWidthC))

  //TODO register in front of control signals
  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numInputA,SInt(portConfig.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB,SInt(portConfig.bitWidthB.W)))
    val propagateOutput: Vec[Vec[Bool]] =  Input(Vec(arrayRow - 1, Vec(arrayCol - 1, Bool())))
    val partialSumReset: Vec[Vec[Bool]] =  Input(Vec(arrayRow, Vec(arrayCol, Bool())))
    val outputC: Vec[SInt] = Output(Vec(numOutput,SInt(portConfig.bitWidthC.W)))
  })

  //Wiring Input
  preProcessorInputA.io.input := io.inputA
  preProcessorInputB.io.input := io.inputB
  systolicTensorArray.io.inputA := preProcessorInputA.io.output
  systolicTensorArray.io.inputB := preProcessorInputB.io.output
  postProcessor.io.input := systolicTensorArray.io.outputC

  //Wiring Control
  systolicTensorArray.io.partialSumReset := RegNext(io.partialSumReset, false.B)
  systolicTensorArray.io.propagateOutput := RegNext(io.propagateOutput, false.B)

  //Wiring Output
  io.outputC := systolicTensorArray.io.outputC

}
