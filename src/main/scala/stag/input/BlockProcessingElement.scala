package stag.input

import chisel3._
import stag.common.PortConfig

class BlockProcessingElement(blockRow: Int, blockCol: Int, peMultiplierCount: Int, flagInputC: Boolean, portConfig: PortConfig) extends Module {

  val numInputA: Int = peMultiplierCount * blockRow
  val numInputB: Int = peMultiplierCount * blockCol
  val numOutput: Int = blockRow

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
    val inputA: Vec[SInt] = Input(Vec(numInputA, SInt(portConfig.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB, SInt(portConfig.bitWidthB.W)))
    val inputC: Option[Vec[SInt]] = if( flagInputC ) Some( Input(Vec(numOutput, SInt(portConfig.bitWidthC.W)))) else None
    val propagateA: Vec[Bool] = Input(Vec(blockRow, Bool()))
    val outputA: Vec[SInt] = Output(Vec(numInputA, SInt(portConfig.bitWidthA.W)))
    val outputB: Vec[SInt] = Output(Vec(numInputB, SInt(portConfig.bitWidthB.W)))
    val outputC: Vec[SInt] = Output(Vec(numOutput, SInt(portConfig.bitWidthC.W)))
  })

  //Wiring Input A
  for ( a <- 0 until blockRow )
    for( p <- 0 until peMultiplierCount )
      processingElementVector(a)(0).io.inputA(p) := io.inputA(a * peMultiplierCount + p)

  //Wiring Output A
  for (a <- 0 until blockRow)
    for (b <- 1 until blockCol)
      for(p <- 0 until peMultiplierCount)
        processingElementVector(a)(b).io.inputA(p) := processingElementVector(a)(b - 1).io.outputA(p)

  for (a <- 0 until blockRow)
    for( p <- 0 until peMultiplierCount)
      io.outputA(a * peMultiplierCount + p) := processingElementVector(a)(blockCol - 1).io.outputA(p)

  //Wiring Input B
  for (a <- 0 until blockRow)
    for (b <- 0 until blockCol)
      for (p <- 0 until peMultiplierCount)
        processingElementVector(a)(b).io.inputB(p) := io.inputB(b * peMultiplierCount + p)

  io.outputB := RegNext(io.inputB, VecInit.fill(numInputB)(0.S))

  //Wiring Control
  for (a <- 0 until blockRow)
    for (b <- 0 until blockCol)
      processingElementVector(a)(b).io.propagateA := io.propagateA(b)

  //Wiring Input C
  if(flagInputC)
    for (a <- 0 until blockRow)
      processingElementVector(a)(0).io.inputC.get := io.inputC.get(a)

  //Wiring Output C
  for (a <- 0 until blockRow)
    for (b <- 1 until blockCol)
      processingElementVector(a)(b).io.inputC.get := processingElementVector(a)(b - 1).io.outputC

  for (a <- 0 until blockRow)
    io.outputC(a) := RegNext(processingElementVector(a)(blockCol - 1).io.outputC, 0.S )

}
