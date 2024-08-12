package stag.output

import chisel3._
import stag.common.PortConfig

class BlockProcessingElement(blockRow: Int, blockCol: Int, numPeMultiplier: Int, flagInputC: Boolean, portConfig: PortConfig) extends Module {

  val numInputA: Int = numPeMultiplier * blockRow
  val numInputB: Int = numPeMultiplier * blockCol
  val numProcessingElement = blockRow * blockCol

  val processingElementVector: Vector[Vector[ProcessingElement]] =
    Vector.fill(blockRow, blockCol)(Module( new ProcessingElement(numPeMultiplier, portConfig)))

  val registerOutputA = RegInit(VecInit(Seq.fill(numInputA)(0.S(portConfig.bitWidthA.W))))
  val registerOutputB = RegInit(VecInit(Seq.fill(numInputB)(0.S(portConfig.bitWidthB.W))))
  val registerOutputC = RegInit(VecInit(Seq.fill(numProcessingElement)(0.S(portConfig.bitWidthC.W))))

  val io = IO(new Bundle {

    //Input
    val inputA: Vec[SInt] = Input(Vec(numInputA, SInt(portConfig.bitWidthA.W) ))
    val inputB: Vec[SInt] = Input(Vec(numInputB, SInt(portConfig.bitWidthB.W) ))
    val inputC: Option[Vec[SInt]] = if(flagInputC) Some(Input(Input(Vec(numProcessingElement, SInt(portConfig.bitWidthC.W))))) else None

    //Control
    val propagateOutput: Option[Bool] = if(flagInputC) Some(Input(Bool())) else None
    val partialSumReset: Bool = Input(Bool())

    //Output
    val outputA: Vec[SInt] = Output(Vec(numInputA, SInt(portConfig.bitWidthA.W)))
    val outputB: Vec[SInt] = Output(Vec(numInputB, SInt(portConfig.bitWidthB.W)))
    val outputC: Vec[SInt] = Output(Vec(numProcessingElement, SInt(portConfig.bitWidthC.W)))

  })

  //Wiring Input A
  for( a <- 0 until blockRow )
    for( b <- 0 until blockCol )
      for( p <-0 until numPeMultiplier )
        processingElementVector(a)(b).io.inputA(p) := io.inputA(a * numPeMultiplier + p)

  registerOutputA := io.inputA

  //Wiring Input B
  for( a <- 0 until blockRow )
    for( b <- 0 until blockCol )
      for( p <- 0 until numPeMultiplier )
        processingElementVector(a)(b).io.inputB(p) := io.inputB(b * numPeMultiplier + p)

  registerOutputB := io.inputB

  //Wiring Control
  for( a <- 0 until blockRow)
    for( b <- 0 until blockCol)
      processingElementVector(a)(b).io.partialSumReset := io.partialSumReset

  //Wiring Input C
  for( a <- 0 until blockRow)
    for( b <- 0 until blockCol){
      val index = a * blockCol + b
      if(flagInputC)
        registerOutputC(index) := Mux(io.propagateOutput.get, io.inputC.get(index), processingElementVector(a)(b).io.output)
      else
        registerOutputC(index) := processingElementVector(a)(b).io.output
    }

  //Wiring Output
  io.outputA := registerOutputA
  io.outputB := registerOutputB
  io.outputC := registerOutputC

}
