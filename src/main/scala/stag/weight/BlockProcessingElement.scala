package stag.weight

import chisel3._
import stag.common.PortConfig

class BlockProcessingElement(blockRow: Int, blockCol: Int, peMultiplierCount: Int, flagInputC: Boolean, portConfig: PortConfig) extends Module {

  val numInputA: Int = peMultiplierCount * blockRow
  val numInputB: Int = peMultiplierCount * blockCol
  val numOutput: Int = blockCol

  val processingElementVector: Vector[Vector[ProcessingElement]] = if(flagInputC) {
    Vector.fill(blockRow, blockCol)(Module(new ProcessingElement(peMultiplierCount, flagInputC = true, portConfig)))
  } else {
    Vector.tabulate(blockRow, blockCol)( (x,_) => if ( x == 0 ){
      Module(new ProcessingElement(peMultiplierCount, flagInputC = false, portConfig))
    } else {
      Module(new ProcessingElement(peMultiplierCount, flagInputC = true, portConfig))
    })
  }

  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numInputA, SInt(portConfig.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB, SInt(portConfig.bitWidthB.W)))
    val inputC: Option[Vec[SInt]] = if( flagInputC ) Some( Input(Vec(numOutput, SInt(portConfig.bitWidthC.W)))) else None
    val propagateB: Vec[Bool] = Input(Vec(blockRow, Bool()))
    val outputA: Vec[SInt] = Output(Vec(numInputA, SInt(portConfig.bitWidthA.W)))
    val outputB: Vec[SInt] = Output(Vec(numInputB, SInt(portConfig.bitWidthB.W)))
    val outputC: Vec[SInt] = Output(Vec(numOutput, SInt(portConfig.bitWidthC.W)))
  })

  //Wiring Input
  //Input A
  for (a <- 0 until blockRow)
    for (b <- 0 until blockCol)
      for (p <- 0 until peMultiplierCount)
        processingElementVector(a)(b).io.inputA(p) := io.inputA(a * peMultiplierCount + p)

  io.outputA := RegNext(io.inputA, VecInit.fill(numInputA)(0.S))

  //Input B
  for( b <- 0 until blockCol )
    for( p <- 0 until peMultiplierCount )
      processingElementVector(0)(b).io.inputB(p) := io.inputB(b * peMultiplierCount + p)


  for( a <- 1 until blockRow )
    for( b <- 0 until blockCol )
      for( p <- 0 until peMultiplierCount )
        processingElementVector(a)(b).io.inputB(p) := processingElementVector(a - 1)(b).io.outputB(p)


  for( b <- 0 until blockCol )
    for( p <- 0 until peMultiplierCount )
      io.outputB(b * peMultiplierCount + p) := processingElementVector(blockRow - 1)(b).io.outputB(p)

  //Wiring Control
  for( a <- 0 until blockRow )
    for( b <- 0 until blockCol )
      processingElementVector(a)(b).io.propagateB := io.propagateB(a)

  //Wiring Output
  if(flagInputC)
    for(b <- 0 until blockCol)
      processingElementVector(0)(b).io.inputC.get := io.inputC.get(b)


  for( a <- 1 until blockRow )
    for( b <- 0 until blockCol )
      processingElementVector(a)(b).io.inputC.get := processingElementVector(a - 1)(b).io.outputC


  for( b <- 0 until blockCol )
    io.outputC(b) := RegNext(processingElementVector(blockRow - 1)(b).io.outputC, 0.S )

}
