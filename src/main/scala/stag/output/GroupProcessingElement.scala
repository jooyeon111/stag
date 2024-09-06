package stag.output

import chisel3._
import stag.common.PortBitWidth

class GroupProcessingElement(vectorPeRow: Int, vectorPeCol: Int, numPeMultiplier: Int, flagInputC: Boolean, portBitWidth: PortBitWidth) extends Module {

  val numInputA: Int = numPeMultiplier * vectorPeRow
  val numInputB: Int = numPeMultiplier * vectorPeCol
  val numProcessingElement = vectorPeRow * vectorPeCol

  val vectorProcessingElementVector: Vector[Vector[VectorProcessingElement]] =
    Vector.fill(vectorPeRow, vectorPeCol)(Module( new VectorProcessingElement(numPeMultiplier, portBitWidth)))

  val registerOutputA = RegInit(VecInit(Seq.fill(numInputA)(0.S(portBitWidth.bitWidthA.W))))
  val registerOutputB = RegInit(VecInit(Seq.fill(numInputB)(0.S(portBitWidth.bitWidthB.W))))
  val registerOutputC = RegInit(VecInit(Seq.fill(numProcessingElement)(0.S(portBitWidth.bitWidthC.W))))

  val io = IO(new Bundle {

    //Input
    val inputA: Vec[SInt] = Input(Vec(numInputA, SInt(portBitWidth.bitWidthA.W) ))
    val inputB: Vec[SInt] = Input(Vec(numInputB, SInt(portBitWidth.bitWidthB.W) ))
    val inputC: Option[Vec[SInt]] = if(flagInputC) Some(Input(Input(Vec(numProcessingElement, SInt(portBitWidth.bitWidthC.W))))) else None

    //Control
    val propagateOutput: Option[Bool] = if(flagInputC) Some(Input(Bool())) else None
    val partialSumReset: Bool = Input(Bool())

    //Output
    val outputA: Vec[SInt] = Output(Vec(numInputA, SInt(portBitWidth.bitWidthA.W)))
    val outputB: Vec[SInt] = Output(Vec(numInputB, SInt(portBitWidth.bitWidthB.W)))
    val outputC: Vec[SInt] = Output(Vec(numProcessingElement, SInt(portBitWidth.bitWidthC.W)))

  })

  //Wiring Input A
  for( a <- 0 until vectorPeRow )
    for( b <- 0 until vectorPeCol )
      for( p <-0 until numPeMultiplier )
        vectorProcessingElementVector(a)(b).io.inputA(p) := io.inputA(a * numPeMultiplier + p)

  registerOutputA := io.inputA

  //Wiring Input B
  for( a <- 0 until vectorPeRow )
    for( b <- 0 until vectorPeCol )
      for( p <- 0 until numPeMultiplier )
        vectorProcessingElementVector(a)(b).io.inputB(p) := io.inputB(b * numPeMultiplier + p)

  registerOutputB := io.inputB

  //Wiring Control
  for( a <- 0 until vectorPeRow)
    for( b <- 0 until vectorPeCol)
      vectorProcessingElementVector(a)(b).io.partialSumReset := io.partialSumReset

  //Wiring Input C
  for( a <- 0 until vectorPeRow)
    for( b <- 0 until vectorPeCol){
      val index = a * vectorPeCol + b
      if(flagInputC)
        registerOutputC(index) := Mux(io.propagateOutput.get, io.inputC.get(index), vectorProcessingElementVector(a)(b).io.output)
      else
        registerOutputC(index) := vectorProcessingElementVector(a)(b).io.output
    }

  //Wiring Output
  io.outputA := registerOutputA
  io.outputB := registerOutputB
  io.outputC := registerOutputC

}
