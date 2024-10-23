package stag.output

import chisel3._
import stag.common.{Arithmetic, PortConfig}

class GroupProcessingElement[T <: Data](
  vectorPeRow: Int,
  vectorPeCol: Int,
  numPeMultiplier: Int,
  withOutputA: Boolean,
  withOutputB: Boolean,
  withInputC: Boolean,
  portConfig: PortConfig[T],
)( implicit ev: Arithmetic[T]) extends Module {

  val numInputA: Int = numPeMultiplier * vectorPeRow
  val numInputB: Int = numPeMultiplier * vectorPeCol
  val numProcessingElement: Int = vectorPeRow * vectorPeCol

  val vectorProcessingElementVector = Vector.fill(vectorPeRow, vectorPeCol)(Module( new VectorProcessingElement(numPeMultiplier, portConfig)))

  val outputTypeC = portConfig.getStaOutputTypeC

  val registerOutputA = RegInit(VecInit(Seq.fill(numInputA)(ev.zero(portConfig.inputTypeA.getWidth))))
  val registerOutputB = RegInit(VecInit(Seq.fill(numInputB)(ev.zero(portConfig.inputTypeB.getWidth))))
  val registerOutputC = RegInit(VecInit(Seq.fill(numProcessingElement)(ev.zero(outputTypeC.getWidth))))

  val io = IO(new Bundle {

    //Input
    val inputA = Input(Vec(numInputA, portConfig.inputTypeA ))
    val inputB = Input(Vec(numInputB, portConfig.inputTypeB ))
    val inputC = if(withInputC) Some(Input(Input(Vec(numProcessingElement, outputTypeC)))) else None

    //Control
    val propagateOutput = if(withInputC) Some(Input(Bool())) else None
    val partialSumReset = Input(Bool())

    //Output
    val outputA = if(withOutputA) Some(Output(Vec(numInputA, portConfig.inputTypeA))) else None
    val outputB = if(withOutputB) Some(Output(Vec(numInputB, portConfig.inputTypeB))) else None
    val outputC = Output(Vec(numProcessingElement, outputTypeC))

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
      if(withInputC)
        registerOutputC(index) := Mux(io.propagateOutput.get, io.inputC.get(index), vectorProcessingElementVector(a)(b).io.output)
      else
        registerOutputC(index) := vectorProcessingElementVector(a)(b).io.output
    }

  //Wiring Output
  if(withOutputA)
    io.outputA.get := registerOutputA

  if(withOutputB)
    io.outputB.get := registerOutputB

  io.outputC := registerOutputC

}
