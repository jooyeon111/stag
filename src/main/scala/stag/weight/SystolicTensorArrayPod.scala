package stag.weight

import chisel3._
import stag.common.{PortConfig, PreProcessor, SystolicTensorArrayConfig}

class SystolicTensorArrayPod(val arrayRow: Int, val arrayCol : Int, val blockRow : Int, val blockCol : Int, val numPeMultiplier : Int, portConfig: PortConfig) extends Module {

  def this(arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig) =
    this(arrayConfig.arrayRow, arrayConfig.arrayCol, arrayConfig.blockRow, arrayConfig.blockCol, arrayConfig.numPeMultiplier, portConfig)

  val numInputA: Int = arrayRow * blockRow * numPeMultiplier
  val numInputB: Int = arrayCol * blockCol * numPeMultiplier
  val numPropagateB: Int = arrayRow * blockRow
  val numOutput : Int = arrayCol * blockCol

  val preProcessorInputA = Module (new PreProcessor(arrayRow, blockRow, numPeMultiplier, skewFlag = true, portConfig.bitWidthA))
  val preProcessorInputB = Module (new PreProcessor(arrayRow, blockRow, numPeMultiplier, skewFlag = false, portConfig.bitWidthA))
  val systolicTensorArray = Module (new SystolicTensorArray(arrayRow, arrayCol, blockRow, blockCol, numPeMultiplier, portConfig, generateRtl = false))
  val postProcessor = Module (new PostProcessor(arrayCol, blockCol, portConfig.bitWidthC))

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
