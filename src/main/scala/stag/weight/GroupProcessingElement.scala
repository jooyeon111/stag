package stag.weight

import chisel3._
import stag.common.{PortConfig, Arithmetic}

class GroupProcessingElement[T <: Data](
  groupPeRowIndex: Int,
  vectorPeRow: Int,
  vectorPeCol: Int,
  numPeMultiplier: Int,
  withInputC: Boolean,
  portConfig: PortConfig[T]
)( implicit ev: Arithmetic[T] ) extends Module {

  val numInputA: Int = numPeMultiplier * vectorPeRow
  val numInputB: Int = numPeMultiplier * vectorPeCol
  val numOutput: Int = vectorPeCol

  val outputTypeC = if(portConfig.enableUserBitWidth)
    portConfig.getStaOutputTypeC
  else
    portConfig.calculateOutputTypeC(portConfig.adderTreeOutputTypeType.getWidth + vectorPeCol + (groupPeRowIndex * vectorPeCol))

  val vectorProcessingElementVector = if(withInputC) {
    Vector.tabulate(vectorPeRow, vectorPeCol)( (vectorPeRowIndex, vectorPeColIndex) => {
      Module(new VectorProcessingElement(groupPeRowIndex, vectorPeRowIndex, vectorPeRow, numPeMultiplier, withInputC = true, portConfig))
    })
  } else {
    Vector.tabulate(vectorPeRow, vectorPeCol)( (vectorPeRowIndex, vectorPeColIndex) => if ( vectorPeRowIndex == 0 ){
      Module(new VectorProcessingElement(groupPeRowIndex, vectorPeRowIndex, vectorPeRow, numPeMultiplier, withInputC = false, portConfig))
    } else {
      Module(new VectorProcessingElement(groupPeRowIndex, vectorPeRowIndex, vectorPeRow, numPeMultiplier, withInputC = true, portConfig))
    })
  }

  val io = IO(new Bundle {
    val inputA = Input(Vec(numInputA, portConfig.inputTypeA))
    val inputB = Input(Vec(numInputB, portConfig.inputTypeB))
    val inputC = if( withInputC ) Some( Input(Vec(numOutput, outputTypeC))) else None
    val propagateB = Input(Vec(vectorPeRow, Bool()))
    val outputA = Output(Vec(numInputA, portConfig.inputTypeA))
    val outputB = Output(Vec(numInputB, portConfig.inputTypeB))
    val outputC = Output(Vec(numOutput, outputTypeC))
  })

  //Wiring Input
  //Input A
  for (a <- 0 until vectorPeRow)
    for (b <- 0 until vectorPeCol)
      for (p <- 0 until numPeMultiplier)
        vectorProcessingElementVector(a)(b).io.inputA(p) := io.inputA(a * numPeMultiplier + p)

  io.outputA := RegNext(io.inputA, VecInit.fill(numInputA)(ev.zero(portConfig.inputTypeA.getWidth)))

  //Input B
  for( b <- 0 until vectorPeCol )
    for( p <- 0 until numPeMultiplier )
      vectorProcessingElementVector(0)(b).io.inputB(p) := io.inputB(b * numPeMultiplier + p)


  for( a <- 1 until vectorPeRow )
    for( b <- 0 until vectorPeCol )
      for( p <- 0 until numPeMultiplier )
        vectorProcessingElementVector(a)(b).io.inputB(p) := vectorProcessingElementVector(a - 1)(b).io.outputB(p)


  for( b <- 0 until vectorPeCol )
    for( p <- 0 until numPeMultiplier )
      io.outputB(b * numPeMultiplier + p) := vectorProcessingElementVector(vectorPeRow - 1)(b).io.outputB(p)

//  println(s" Group Row Index ${groupPeRowIndex}")
  //Wiring Control
  for( a <- 0 until vectorPeRow )
    for( b <- 0 until vectorPeCol ) {
      vectorProcessingElementVector(a)(b).io.propagateB := io.propagateB(a)
//      println(s"vectorProcessingElementVector($a)($b).io.propagateB := io.propagateB($a)")
    }

  //Wiring Output
  if(withInputC)
    for(b <- 0 until vectorPeCol)
      vectorProcessingElementVector(0)(b).io.inputC.get := io.inputC.get(b)


  for( a <- 1 until vectorPeRow )
    for( b <- 0 until vectorPeCol )
      vectorProcessingElementVector(a)(b).io.inputC.get := vectorProcessingElementVector(a - 1)(b).io.outputC


  for( b <- 0 until vectorPeCol )
    io.outputC(b) := RegNext(vectorProcessingElementVector(vectorPeRow - 1)(b).io.outputC, ev.zero(portConfig.inputTypeB.getWidth) )

}
