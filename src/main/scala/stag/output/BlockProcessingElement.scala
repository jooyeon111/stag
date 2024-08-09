package stag.output

import chisel3._
import stag.sub.PortConfig

class BlockProcessingElement(blockRow: Int, blockCol: Int, numPeMultiplier: Int, flagInputC: Boolean, portConfig: PortConfig) extends Module {

  val numInputA: Int = numPeMultiplier * blockRow
  val numInputB: Int = numPeMultiplier * blockCol
  val numProcessingElement = blockRow * blockCol

  val processingElementVector: Vector[Vector[ProcessingElement]] =
    Vector.fill(blockRow, blockCol)(Module( new ProcessingElement(numPeMultiplier, portConfig)))

  val registerOutputA = RegInit(VecInit(Seq.fill(numInputA)(0.S(portConfig.bitWidthA))))
  val registerOutputB = RegInit(VecInit(Seq.fill(numInputB)(0.S(portConfig.bitWidthB))))
  val registerOutputC = RegInit(VecInit(Seq.fill(numProcessingElement)(0.S(portConfig.bitWidthC))))

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

  //Wiring Input
  for( i <- 0 until blockRow)
    for(j <- 0 until blockCol)
      for(k <-0 until numPeMultiplier)
        processingElementVector(i)(j).io.inputA(k) := io.inputA(i * numPeMultiplier + k)

  for( i <- 0 until blockRow)
    for(j <- 0 until blockCol)
      for(k <-0 until numPeMultiplier)
        processingElementVector(i)(j).io.inputB(k) := io.inputB(j * numPeMultiplier + k)

  //Wiring Control
  for( i <- 0 until blockRow)
    for( j <- 0 until blockCol)
      processingElementVector(i)(j).io.partialSumReset := io.partialSumReset

  //Wiring Output

  registerOutputA := io.inputA
  registerOutputB := io.inputB

  for (i <- 0 until blockRow; j <- 0 until blockCol){
    val index = i * blockCol + j
    if(flagInputC){
      registerOutputC(index) := Mux(io.propagateOutput.get, io.inputC.get(index), processingElementVector(i)(j).io.output)
    }else{
      registerOutputC(index) := processingElementVector(i)(j).io.output
    }
  }

 io.outputA := registerOutputA
 io.outputB := registerOutputB
 io.outputC := registerOutputC

}
