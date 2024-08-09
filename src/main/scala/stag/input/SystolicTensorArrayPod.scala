package stag.input

import chisel3._
import stag.sub.{PortConfig, PreProcessor, SystolicTensorArrayConfig}


class SystolicTensorArrayPod(val arrayRow: Int, val arrayCol : Int, val blockRow : Int, val blockCol : Int, val numPeMultiplier : Int, portConfig: PortConfig) extends Module {

  def this(arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig) =
    this(arrayConfig.arrayRow, arrayConfig.arrayCol, arrayConfig.blockRow, arrayConfig.blockCol, arrayConfig.numPeMultiplier, portConfig)

  val numInputA: Int = arrayRow * blockRow * numPeMultiplier
  val numInputB: Int = arrayCol * blockCol * numPeMultiplier
  val numOutput : Int = arrayRow * blockRow

  val preProcessorInputA = Module (new PreProcessor(arrayRow, blockRow, numPeMultiplier, skewFlag = false, portConfig.bitWidthA))
  val preProcessorInputB = Module (new PreProcessor(arrayRow, blockRow, numPeMultiplier, skewFlag = true, portConfig.bitWidthA))
  val systolicTensorArray = Module (new SystolicTensorArray(arrayRow, arrayCol, blockRow, blockCol, numPeMultiplier, portConfig))

  val io = IO(new Bundle {

    //Input
    val inputA: Vec[SInt] = Input(Vec(numInputA, SInt(portConfig.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB, SInt(portConfig.bitWidthB.W)))

    //Control
    val propagateA : Vec[Bool] = Input(Vec(arrayRow, Bool()))

    //Output
    val outputC: Vec[SInt] = Output(Vec(numOutput, SInt(portConfig.bitWidthC.W)))

  })

  //Wiring Input
  preProcessorInputA.io.input := io.inputA
  preProcessorInputB.io.input := io.inputB
  systolicTensorArray.io.inputA := preProcessorInputA.io.output
  systolicTensorArray.io.inputB := preProcessorInputB.io.output

  //Wiring Control
  systolicTensorArray.io.propagateA := io.propagateA

  //Wiring Output
  io.outputC := systolicTensorArray.io.outputC

}
