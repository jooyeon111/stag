package stag.input

import chisel3._
import stag.sub.PortConfig

class BlockProcessingElement(blockRow: Int, blockCol: Int, peMultiplierCount: Int, flagInputC: Boolean, portConfig: PortConfig) extends Module {

  val numInputA: Int = peMultiplierCount * blockRow
  val numInputB: Int = peMultiplierCount * blockCol
  val numOutput: Int = blockCol

  val processingElementVector: Vector[Vector[ProcessingElement]] = if(flagInputC) {
    Vector.fill(blockRow, blockCol)(Module(new ProcessingElement(peMultiplierCount, flagInputC = true, portConfig)))
  } else {
    Vector.tabulate(blockRow, blockCol)( (_,y) => if ( y == 0 ){
      Module(new ProcessingElement(peMultiplierCount, flagInputC = false, portConfig))
    } else {
      Module(new ProcessingElement(peMultiplierCount, flagInputC = true, portConfig))
    })
  }

  val io = IO(new Bundle {

    //Input
    val inputA: Vec[SInt] = Input(Vec(numInputA, SInt(portConfig.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB, SInt(portConfig.bitWidthB.W)))
    val inputC: Option[Vec[SInt]] = if( flagInputC ) Some( Input(Vec(numOutput, SInt(portConfig.bitWidthC.W)))) else None

    //Control
    val propagateA : Bool =  Input(Bool())

    //Output
    val outputA: Vec[SInt] = Output(Vec(numInputA, SInt(portConfig.bitWidthA.W)))
    val outputB: Vec[SInt] = Output(Vec(numInputB, SInt(portConfig.bitWidthB.W)))
    val outputC: Vec[SInt] = Output(Vec(numOutput, SInt(portConfig.bitWidthC.W)))

  })

  //Wiring Input

  //Input A
  for (j <- 0 until blockCol)
    for( k <- 0 until peMultiplierCount)
      processingElementVector(0)(j).io.inputA(k) := io.inputA(j * peMultiplierCount + k)


  for (i <- 1 until blockRow)
    for (j <- 0 until blockCol)
      for(k <- 0 until peMultiplierCount)
        processingElementVector(i)(j).io.inputB(k) := processingElementVector(i - 1)(j).io.outputA(k)


  for (j <- 0 until blockCol)
    for( k <- 0 until peMultiplierCount)
      io.outputA(j * peMultiplierCount + k) := processingElementVector(blockRow - 1)(j).io.outputA(k)

  //Input B
  for (i <- 0 until blockRow)
    for (j <- 0 until blockCol)
      for (k <- 0 until peMultiplierCount)
        processingElementVector(i)(j).io.inputB(k) := io.inputB(i * peMultiplierCount + k)

  io.outputB := RegNext(io.inputB, VecInit.fill(numInputB)(0.S))

  //Wiring Control
  for (i <- 0 until blockRow)
    for (j <- 0 until blockCol)
      processingElementVector(i)(j).io.propagateA := io.propagateA

  //Wiring Output
  for (j <- 0 until blockCol)
    if(flagInputC)
      processingElementVector(0)(j).io.inputC.get := io.inputC.get(j)

  for (i <- 1 until blockRow)
    for (j <- 0 until blockCol) {
      processingElementVector(i)(j).io.inputC.get := processingElementVector(i - 1)(j).io.outputC
    }

  for (j <- 0 until blockCol)
    io.outputC(j) := RegNext(processingElementVector(blockRow - 1)(j).io.outputC, 0.S )




}
