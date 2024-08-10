package stag.input

import chisel3._
import stag.sub.SystolicTensorArrayConfig
import stag.sub.PortConfig

class SystolicTensorArray(val arrayRow: Int, val arrayCol : Int, val blockRow : Int, val blockCol : Int, val numPeMultiplier : Int, portConfig: PortConfig) extends Module {
  def this(arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig) =
    this(arrayConfig.arrayRow, arrayConfig.arrayCol, arrayConfig.blockRow, arrayConfig.blockCol, arrayConfig.numPeMultiplier, portConfig)

  val numInputA: Int = arrayRow * blockRow * numPeMultiplier
  val numInputB: Int = arrayCol * blockCol * numPeMultiplier
  val numOutput : Int = arrayRow * blockRow

  val blockProcessingElementVector: Vector[Vector[BlockProcessingElement]] = Vector.tabulate(arrayRow, arrayCol)((_,y) => if ( y == 0 ) {
    Module(new BlockProcessingElement(blockRow, blockCol, numPeMultiplier, flagInputC = false, portConfig))
  } else{
    Module(new BlockProcessingElement(blockRow, blockCol, numPeMultiplier, flagInputC = true, portConfig))
  })

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
  //Wiring Input A
  for (i <- 0 until arrayRow)
    for (k <- 0 until blockRow * numPeMultiplier)
      blockProcessingElementVector(i)(0).io.inputA(k) := io.inputA(k + (i * blockRow * numPeMultiplier))


  for (i <- 0 until arrayRow)
    for (j <- 1 until arrayCol)
      for (k <- 0 until blockRow * numPeMultiplier)
        blockProcessingElementVector(i)(j).io.inputA(k) := blockProcessingElementVector(i)(j - 1).io.outputA(k)

  //Wiring Input B
  for (i <- 0 until arrayCol)
    for (k <- 0 until blockCol * numPeMultiplier)
      blockProcessingElementVector(0)(i).io.inputB(k) := io.inputB(k + (i * blockCol * numPeMultiplier))


  for (i <- 1 until arrayRow)
    for (j <- 0 until arrayCol)
      for (k <- 0 until blockCol * numPeMultiplier)
        blockProcessingElementVector(i)(j).io.inputB(k) := blockProcessingElementVector(i - 1)(j).io.outputB(k)

  //Wiring Control
  for (i <- 0 until arrayRow)
    for (j <- 0 until arrayCol)
      blockProcessingElementVector(i)(j).io.propagateA := io.propagateA(i)

  //Wiring Output
  for (i <- 1 until arrayCol)
    for (j <- 0 until arrayRow)
      for (k <- 0 until blockRow)
        blockProcessingElementVector(i)(j).io.inputC.get(k) := blockProcessingElementVector(i)(j - 1).io.outputC(k)

  for (i <- 0 until arrayRow)
    for (k <- 0 until blockRow)
      io.outputC(k + (i * blockRow)) := blockProcessingElementVector(i)(arrayCol - 1).io.outputC(k)


}
