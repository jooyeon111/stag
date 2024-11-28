package stag.weight

import chisel3._
import chisel3.util.log2Ceil
import stag.common.{PortConfig, Arithmetic, VerilogNaming}

class GroupProcessingElement[T <: Data](
  groupPeRowIndex: Int,
  vectorPeRow: Int,
  vectorPeCol: Int,
  numPeMultiplier: Int,
  withOutputA: Boolean,
  withOutputB: Boolean,
  withInputC: Boolean,
  portConfig: PortConfig[T]
)( implicit ev: Arithmetic[T] ) extends Module with VerilogNaming with ProcessingElementIo[T] {

  override val desiredName:String = camelToSnake(this.getClass.getSimpleName)

  val numInputA: Int = numPeMultiplier * vectorPeRow
  val numInputB: Int = numPeMultiplier * vectorPeCol
  val numOutput: Int = vectorPeCol

  val outputTypeC = if(portConfig.enableUserBitWidth)
    portConfig.getStaOutputTypeC
  else
    portConfig.calculateOutputTypeC(
      portConfig.adderTreeOutputTypeType.getWidth + log2Ceil(vectorPeRow + groupPeRowIndex * vectorPeRow)
    )

  val vectorProcessingElementVector = Vector.tabulate(vectorPeRow, vectorPeCol) { (vectorPeRowIndex, _) =>
    // Determine position flags
    val isFirstRow = vectorPeRowIndex == 0
    val isLastRow = vectorPeRowIndex == vectorPeRow - 1

    // Calculate PE configuration based on position and input parameters
    val peWithOutputB = if (!withOutputB && isLastRow) false else true
    val peWithInputC = if (!withInputC && isFirstRow) false else true

    Module(new VectorProcessingElement(
      groupPeRowIndex = groupPeRowIndex,
      vectorPeRowIndex = vectorPeRowIndex,
      vectorPeRow = vectorPeRow,
      numPeMultiplier = numPeMultiplier,
      withOutputB = peWithOutputB,
      withInputC = peWithInputC,
      withOutputA = false,
      portConfig = portConfig
    ))

  }

  override type OutputType = Vec[T]
  override type PropagateType = Vec[Bool]

  val io = IO(new Bundle {
    val inputA = Input(Vec(numInputA, portConfig.inputTypeA))
    val inputB = Input(Vec(numInputB, portConfig.inputTypeB))
    val inputC = if( withInputC ) Some( Input(Vec(numOutput, outputTypeC))) else None

    val propagateB = Input(Vec(vectorPeRow, Bool()))

    val outputA = if(withOutputA) Some (Output(Vec(numInputA, portConfig.inputTypeA))) else None
    val outputB = if(withOutputB) Some (Output(Vec(numInputB, portConfig.inputTypeB))) else None
    val outputC = Output(Vec(numOutput, outputTypeC))
  })

  //Wiring Input
  //Input A
  for (a <- 0 until vectorPeRow)
    for (b <- 0 until vectorPeCol)
      for (p <- 0 until numPeMultiplier)
        vectorProcessingElementVector(a)(b).io.inputA(p) := io.inputA(a * numPeMultiplier + p)

  if(withOutputA)
    io.outputA.get := RegNext(io.inputA, VecInit.fill(numInputA)(ev.zero(portConfig.inputTypeA.getWidth)))

  //Input B
  for( b <- 0 until vectorPeCol )
    for( p <- 0 until numPeMultiplier )
      vectorProcessingElementVector(0)(b).io.inputB(p) := io.inputB(b * numPeMultiplier + p)


  for( a <- 1 until vectorPeRow )
    for( b <- 0 until vectorPeCol )
      for( p <- 0 until numPeMultiplier )
        vectorProcessingElementVector(a)(b).io.inputB(p) := vectorProcessingElementVector(a - 1)(b).io.outputB.get(p)


  if(withOutputB)
    for( b <- 0 until vectorPeCol )
      for( p <- 0 until numPeMultiplier )
        io.outputB.get(b * numPeMultiplier + p) := vectorProcessingElementVector(vectorPeRow - 1)(b).io.outputB.get(p)

  //Wiring Control
  for( a <- 0 until vectorPeRow )
    for( b <- 0 until vectorPeCol )
      vectorProcessingElementVector(a)(b).io.propagateB := io.propagateB(a)

  //Wiring Output
  if(withInputC)
    for(b <- 0 until vectorPeCol)
      vectorProcessingElementVector(0)(b).io.inputC.get := io.inputC.get(b)


  for( a <- 1 until vectorPeRow )
    for( b <- 0 until vectorPeCol )
      vectorProcessingElementVector(a)(b).io.inputC.get := vectorProcessingElementVector(a - 1)(b).io.outputC


  for( b <- 0 until vectorPeCol )
    io.outputC(b) := RegNext(vectorProcessingElementVector(vectorPeRow - 1)(b).io.outputC, ev.zero(outputTypeC.getWidth) )

}
