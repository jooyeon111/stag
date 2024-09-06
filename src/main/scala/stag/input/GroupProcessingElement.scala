package stag.input

import chisel3._
import stag.common.PortBitWidth

class GroupProcessingElement(vectorPeRow: Int, vectorPeCol: Int, peMultiplierCount: Int, flagInputC: Boolean, portBitWidth: PortBitWidth) extends Module {

  val numInputA: Int = peMultiplierCount * vectorPeRow
  val numInputB: Int = peMultiplierCount * vectorPeCol
  val numOutput: Int = vectorPeRow

  val vectorProcessingElementVector: Vector[Vector[VectorProcessingElement]] = if(flagInputC) {
    Vector.fill(vectorPeRow, vectorPeCol)(Module(new VectorProcessingElement(peMultiplierCount, flagInputC = true, portBitWidth)))
  } else {
    Vector.tabulate(vectorPeRow, vectorPeCol)( (_,y) => if ( y == 0 ){
      Module(new VectorProcessingElement(peMultiplierCount, flagInputC = false, portBitWidth))
    } else {
      Module(new VectorProcessingElement(peMultiplierCount, flagInputC = true, portBitWidth))
    })
  }

  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numInputA, SInt(portBitWidth.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB, SInt(portBitWidth.bitWidthB.W)))
    val inputC: Option[Vec[SInt]] = if( flagInputC ) Some( Input(Vec(numOutput, SInt(portBitWidth.bitWidthC.W)))) else None
    val propagateA: Vec[Bool] = Input(Vec(vectorPeRow, Bool()))
    val outputA: Vec[SInt] = Output(Vec(numInputA, SInt(portBitWidth.bitWidthA.W)))
    val outputB: Vec[SInt] = Output(Vec(numInputB, SInt(portBitWidth.bitWidthB.W)))
    val outputC: Vec[SInt] = Output(Vec(numOutput, SInt(portBitWidth.bitWidthC.W)))
  })

  //Wiring Input A
  for ( a <- 0 until vectorPeRow )
    for( p <- 0 until peMultiplierCount )
      vectorProcessingElementVector(a)(0).io.inputA(p) := io.inputA(a * peMultiplierCount + p)

  //Wiring Output A
  for (a <- 0 until vectorPeRow)
    for (b <- 1 until vectorPeCol)
      for(p <- 0 until peMultiplierCount)
        vectorProcessingElementVector(a)(b).io.inputA(p) := vectorProcessingElementVector(a)(b - 1).io.outputA(p)

  for (a <- 0 until vectorPeRow)
    for( p <- 0 until peMultiplierCount)
      io.outputA(a * peMultiplierCount + p) := vectorProcessingElementVector(a)(vectorPeCol - 1).io.outputA(p)

  //Wiring Input B
  for (a <- 0 until vectorPeRow)
    for (b <- 0 until vectorPeCol)
      for (p <- 0 until peMultiplierCount)
        vectorProcessingElementVector(a)(b).io.inputB(p) := io.inputB(b * peMultiplierCount + p)

  io.outputB := RegNext(io.inputB, VecInit.fill(numInputB)(0.S))

  //Wiring Control
  for (a <- 0 until vectorPeRow)
    for (b <- 0 until vectorPeCol)
      vectorProcessingElementVector(a)(b).io.propagateA := io.propagateA(b)

  //Wiring Input C
  if(flagInputC)
    for (a <- 0 until vectorPeRow)
      vectorProcessingElementVector(a)(0).io.inputC.get := io.inputC.get(a)

  //Wiring Output C
  for (a <- 0 until vectorPeRow)
    for (b <- 1 until vectorPeCol)
      vectorProcessingElementVector(a)(b).io.inputC.get := vectorProcessingElementVector(a)(b - 1).io.outputC

  for (a <- 0 until vectorPeRow)
    io.outputC(a) := RegNext(vectorProcessingElementVector(a)(vectorPeCol - 1).io.outputC, 0.S )

}
