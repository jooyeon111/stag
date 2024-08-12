package stag.input

import chisel3._
import stag.common.{PortConfig, PreProcessor, SystolicTensorArrayConfig}


class SystolicTensorArrayPod(val arrayRow: Int, val arrayCol : Int, val blockRow : Int, val blockCol : Int, val numPeMultiplier : Int, portConfig: PortConfig) extends Module {

  def this(arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig) =
    this(arrayConfig.arrayRow, arrayConfig.arrayCol, arrayConfig.blockRow, arrayConfig.blockCol, arrayConfig.numPeMultiplier, portConfig)

  val numInputA: Int = arrayRow * blockRow * numPeMultiplier
  val numInputB: Int = arrayCol * blockCol * numPeMultiplier
  val numOutput : Int = arrayRow * blockRow

  val preProcessorInputA = Module (new PreProcessor(arrayRow, blockRow, numPeMultiplier, skewFlag = false, portConfig.bitWidthA))
  val preProcessorInputB = Module (new PreProcessor(arrayRow, blockRow, numPeMultiplier, skewFlag = true, portConfig.bitWidthA))
  val systolicTensorArray = Module (new SystolicTensorArray(arrayRow, arrayCol, blockRow, blockCol, numPeMultiplier, portConfig, generateRtl = false))
  val postProcessor = Module ( new PostProcessor(arrayRow, blockRow, portConfig.bitWidthC))

  //TODO register in front of control signals
  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numInputA, SInt(portConfig.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB, SInt(portConfig.bitWidthB.W)))
    val propagateA : Vec[Bool] = Input(Vec(arrayRow, Bool()))
    val outputC: Vec[SInt] = Output(Vec(numOutput, SInt(portConfig.bitWidthC.W)))
  })

  //Wiring Input
  preProcessorInputA.io.input := io.inputA
  preProcessorInputB.io.input := io.inputB
  systolicTensorArray.io.inputA := preProcessorInputA.io.output
  systolicTensorArray.io.inputB := preProcessorInputB.io.output
  postProcessor.io.input := systolicTensorArray.io.outputC

  //Wiring Control
  systolicTensorArray.io.propagateA := RegNext(io.propagateA, false.B)

  //Wiring Output
  io.outputC := systolicTensorArray.io.outputC

}
