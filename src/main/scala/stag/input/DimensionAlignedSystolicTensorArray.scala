package stag.input

import chisel3._
import stag.common.{PortBitWidth, PreProcessor, SystolicTensorArrayConfig}


class DimensionAlignedSystolicTensorArray(val groupPeRow: Int, val groupPeCol : Int, val vectorPeRow : Int, val vectorPeCol : Int, val numPeMultiplier : Int, portBitWidth: PortBitWidth) extends Module {

  def this(arrayConfig: SystolicTensorArrayConfig, portBitWidth: PortBitWidth) =
    this(arrayConfig.groupPeRow, arrayConfig.groupPeCol, arrayConfig.vectorPeRow, arrayConfig.vectorPeCol, arrayConfig.numPeMultiplier, portBitWidth)

  val numInputA: Int = groupPeRow * vectorPeRow * numPeMultiplier
  val numInputB: Int = groupPeCol * vectorPeCol * numPeMultiplier
  val numPropagateA: Int = groupPeCol * vectorPeCol
  val numOutput : Int = groupPeRow * vectorPeRow

  val preProcessorInputA = Module (new PreProcessor(groupPeRow, vectorPeRow, numPeMultiplier, skewFlag = false, portBitWidth.bitWidthA))
  val preProcessorInputB = Module (new PreProcessor(groupPeRow, vectorPeRow, numPeMultiplier, skewFlag = true, portBitWidth.bitWidthA))
  val systolicTensorArray = Module (new SystolicTensorArray(groupPeRow, groupPeCol, vectorPeRow, vectorPeCol, numPeMultiplier, portBitWidth, generateRtl = false))
  val postProcessor = Module ( new PostProcessor(groupPeRow, vectorPeRow, portBitWidth.bitWidthC))

  // register in front of control signals
  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numInputA, SInt(portBitWidth.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB, SInt(portBitWidth.bitWidthB.W)))
    val propagateA : Vec[Bool] = Input(Vec(numPropagateA, Bool()))
    val outputC: Vec[SInt] = Output(Vec(numOutput, SInt(portBitWidth.bitWidthC.W)))
  })

  //Wiring Input
  preProcessorInputA.io.input := io.inputA
  preProcessorInputB.io.input := io.inputB
  systolicTensorArray.io.inputA := preProcessorInputA.io.output
  systolicTensorArray.io.inputB := preProcessorInputB.io.output
  postProcessor.io.input := systolicTensorArray.io.outputC

  //Wiring Control
  systolicTensorArray.io.propagateA := RegNext(io.propagateA, VecInit.fill(numPropagateA)(false.B))

  //Wiring Output
  io.outputC := systolicTensorArray.io.outputC

}
