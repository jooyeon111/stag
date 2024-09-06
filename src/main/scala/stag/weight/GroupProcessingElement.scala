package stag.weight

import chisel3._
import stag.common.PortBitWidth

class GroupProcessingElement(vectorPeRow: Int, vectorPeCol: Int, peMultiplierCount: Int, flagInputC: Boolean, portBitWidth: PortBitWidth) extends Module {

  val numInputA: Int = peMultiplierCount * vectorPeRow
  val numInputB: Int = peMultiplierCount * vectorPeCol
  val numOutput: Int = vectorPeCol

  val vectorProcessingElementVector: Vector[Vector[VectorProcessingElement]] = if(flagInputC) {
    Vector.fill(vectorPeRow, vectorPeCol)(Module(new VectorProcessingElement(peMultiplierCount, flagInputC = true, portBitWidth)))
  } else {
    Vector.tabulate(vectorPeRow, vectorPeCol)( (x,_) => if ( x == 0 ){
      Module(new VectorProcessingElement(peMultiplierCount, flagInputC = false, portBitWidth))
    } else {
      Module(new VectorProcessingElement(peMultiplierCount, flagInputC = true, portBitWidth))
    })
  }

  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numInputA, SInt(portBitWidth.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB, SInt(portBitWidth.bitWidthB.W)))
    val inputC: Option[Vec[SInt]] = if( flagInputC ) Some( Input(Vec(numOutput, SInt(portBitWidth.bitWidthC.W)))) else None
    val propagateB: Vec[Bool] = Input(Vec(vectorPeRow, Bool()))
    val outputA: Vec[SInt] = Output(Vec(numInputA, SInt(portBitWidth.bitWidthA.W)))
    val outputB: Vec[SInt] = Output(Vec(numInputB, SInt(portBitWidth.bitWidthB.W)))
    val outputC: Vec[SInt] = Output(Vec(numOutput, SInt(portBitWidth.bitWidthC.W)))
  })

  //Wiring Input
  //Input A
  for (a <- 0 until vectorPeRow)
    for (b <- 0 until vectorPeCol)
      for (p <- 0 until peMultiplierCount)
        vectorProcessingElementVector(a)(b).io.inputA(p) := io.inputA(a * peMultiplierCount + p)

  io.outputA := RegNext(io.inputA, VecInit.fill(numInputA)(0.S))

  //Input B
  for( b <- 0 until vectorPeCol )
    for( p <- 0 until peMultiplierCount )
      vectorProcessingElementVector(0)(b).io.inputB(p) := io.inputB(b * peMultiplierCount + p)


  for( a <- 1 until vectorPeRow )
    for( b <- 0 until vectorPeCol )
      for( p <- 0 until peMultiplierCount )
        vectorProcessingElementVector(a)(b).io.inputB(p) := vectorProcessingElementVector(a - 1)(b).io.outputB(p)


  for( b <- 0 until vectorPeCol )
    for( p <- 0 until peMultiplierCount )
      io.outputB(b * peMultiplierCount + p) := vectorProcessingElementVector(vectorPeRow - 1)(b).io.outputB(p)

  //Wiring Control
  for( a <- 0 until vectorPeRow )
    for( b <- 0 until vectorPeCol )
      vectorProcessingElementVector(a)(b).io.propagateB := io.propagateB(a)

  //Wiring Output
  if(flagInputC)
    for(b <- 0 until vectorPeCol)
      vectorProcessingElementVector(0)(b).io.inputC.get := io.inputC.get(b)


  for( a <- 1 until vectorPeRow )
    for( b <- 0 until vectorPeCol )
      vectorProcessingElementVector(a)(b).io.inputC.get := vectorProcessingElementVector(a - 1)(b).io.outputC


  for( b <- 0 until vectorPeCol )
    io.outputC(b) := RegNext(vectorProcessingElementVector(vectorPeRow - 1)(b).io.outputC, 0.S )

}
