package stag.input

import chisel3._
import chisel3.util.log2Ceil
import stag.common.{Arithmetic, PortConfig}

class GroupProcessingElement[T <: Data](
  groupPeColIndex: Int,
  vectorPeRow: Int,
  vectorPeCol: Int,
  numPeMultiplier: Int,
  withOutputA: Boolean,
  withOutputB: Boolean,
  withInputC: Boolean,
  portConfig: PortConfig[T]
)(implicit ev: Arithmetic[T]) extends Module {

  val numInputA: Int = numPeMultiplier * vectorPeRow
  val numInputB: Int = numPeMultiplier * vectorPeCol
  val numOutput: Int = vectorPeRow

  val outputTypeC = if (portConfig.enableUserBitWidth)
    portConfig.getStaOutputTypeC
  else
    portConfig.calculateOutputTypeC(portConfig.adderTreeOutputTypeType.getWidth + log2Ceil(vectorPeCol + groupPeColIndex * vectorPeCol) )

  val vectorProcessingElementVector = (withOutputA, withInputC) match {
    case (false, false) =>
      Vector.tabulate(vectorPeRow, vectorPeCol){ (_, vectorPeColIndex) => {

        if(vectorPeColIndex == 0){
          Module(new VectorProcessingElement(
            groupPeColIndex,
            vectorPeColIndex,
            vectorPeCol,
            numPeMultiplier,
            withOutputA = true,
            withInputC = false,
            portConfig
          ))
        } else if ( 0 < vectorPeColIndex && vectorPeColIndex < vectorPeCol - 1){
          Module(new VectorProcessingElement(
            groupPeColIndex,
            vectorPeColIndex,
            vectorPeCol,
            numPeMultiplier,
            withOutputA = true,
            withInputC = true,
            portConfig
          ))
        } else {
          Module(new VectorProcessingElement(
            groupPeColIndex,
            vectorPeColIndex,
            vectorPeCol,
            numPeMultiplier,
            withOutputA = false,
            withInputC = true,
            portConfig
          ))
        }

      }}
    case (false, true) =>
      Vector.tabulate(vectorPeRow, vectorPeCol){ (_, vectorPeColIndex) => {

        if(vectorPeColIndex == 0){
          Module(new VectorProcessingElement(
            groupPeColIndex,
            vectorPeColIndex,
            vectorPeCol,
            numPeMultiplier,
            withOutputA = true,
            withInputC = true,
            portConfig
          ))
        } else if ( 0 < vectorPeColIndex && vectorPeColIndex < vectorPeCol - 1){
          Module(new VectorProcessingElement(
            groupPeColIndex,
            vectorPeColIndex,
            vectorPeCol,
            numPeMultiplier,
            withOutputA = true,
            withInputC = true,
            portConfig
          ))
        } else {
          Module(new VectorProcessingElement(
            groupPeColIndex,
            vectorPeColIndex,
            vectorPeCol,
            numPeMultiplier,
            withOutputA = false,
            withInputC = true,
            portConfig
          ))
        }

      }}
    case (true, false) =>
      Vector.tabulate(vectorPeRow, vectorPeCol){ (_, vectorPeColIndex) => {

        if(vectorPeColIndex == 0){
          Module(new VectorProcessingElement(
            groupPeColIndex,
            vectorPeColIndex,
            vectorPeCol,
            numPeMultiplier,
            withOutputA = true,
            withInputC = false,
            portConfig
          ))

        } else if ( 0 < vectorPeColIndex && vectorPeColIndex < vectorPeCol - 1){
          Module(new VectorProcessingElement(
            groupPeColIndex,
            vectorPeColIndex,
            vectorPeCol,
            numPeMultiplier,
            withOutputA = true,
            withInputC = true,
            portConfig
          ))
        } else {
          Module(new VectorProcessingElement(
            groupPeColIndex,
            vectorPeColIndex,
            vectorPeCol,
            numPeMultiplier,
            withOutputA = true,
            withInputC = true,
            portConfig
          ))
        }
      }}
    case (true, true) =>
      Vector.tabulate(vectorPeRow, vectorPeCol){ (_, vectorPeColIndex) => {
        if(vectorPeColIndex == 0){
          Module(new VectorProcessingElement(
            groupPeColIndex,
            vectorPeColIndex,
            vectorPeCol,
            numPeMultiplier,
            withOutputA = true,
            withInputC = true,
            portConfig
          ))
        } else if ( 0 < vectorPeColIndex && vectorPeColIndex < vectorPeCol - 1){
          Module(new VectorProcessingElement(
            groupPeColIndex,
            vectorPeColIndex,
            vectorPeCol,
            numPeMultiplier,
            withOutputA = true,
            withInputC = true,
            portConfig
          ))
        } else {
          Module(new VectorProcessingElement(
            groupPeColIndex,
            vectorPeColIndex,
            vectorPeCol,
            numPeMultiplier,
            withOutputA = true,
            withInputC = true,
            portConfig
          ))
        }

      }}
  }

  val io = IO(new Bundle {

    val inputA = Input(Vec(numInputA, portConfig.inputTypeA))
    val inputB = Input(Vec(numInputB, portConfig.inputTypeB))
    val inputC = if( withInputC ) Some( Input(Vec(numOutput, outputTypeC)) ) else None

    val propagateA = Input(Vec(vectorPeCol, Bool()))

    val outputA = if(withOutputA) Some(Output(Vec(numInputA, portConfig.inputTypeA))) else None
    val outputB = if(withOutputB) Some(Output(Vec(numInputB, portConfig.inputTypeB))) else None
    val outputC = Output(Vec(numOutput, outputTypeC))
  })

  //Wiring Input A
  for ( a <- 0 until vectorPeRow )
    for( p <- 0 until numPeMultiplier )
      vectorProcessingElementVector(a)(0).io.inputA(p) := io.inputA(a * numPeMultiplier + p)

  //Wiring Output A
  for (a <- 0 until vectorPeRow)
    for (b <- 1 until vectorPeCol)
      for(p <- 0 until numPeMultiplier)
        vectorProcessingElementVector(a)(b).io.inputA(p) := vectorProcessingElementVector(a)(b - 1).io.outputA.get(p)

  if(withOutputA)
    for (a <- 0 until vectorPeRow)
      for( p <- 0 until numPeMultiplier)
        io.outputA.get(a * numPeMultiplier + p) := vectorProcessingElementVector(a)(vectorPeCol - 1).io.outputA.get(p)

  //Wiring Input B
  for (a <- 0 until vectorPeRow)
    for (b <- 0 until vectorPeCol)
      for (p <- 0 until numPeMultiplier)
        vectorProcessingElementVector(a)(b).io.inputB(p) := io.inputB(b * numPeMultiplier + p)

  if(withOutputB)
    io.outputB.get := RegNext(io.inputB, VecInit.fill(numInputB)(ev.zero(portConfig.inputTypeB.getWidth)))

  //Wiring Control
  for (a <- 0 until vectorPeRow)
    for (b <- 0 until vectorPeCol)
      vectorProcessingElementVector(a)(b).io.propagateA := io.propagateA(b)

  //Wiring Input C
  if(withInputC)
    for (a <- 0 until vectorPeRow)
      vectorProcessingElementVector(a)(0).io.inputC.get := io.inputC.get(a)

  //Wiring Output C
  for (a <- 0 until vectorPeRow)
    for (b <- 1 until vectorPeCol)
      vectorProcessingElementVector(a)(b).io.inputC.get := vectorProcessingElementVector(a)(b - 1).io.outputC

  for (a <- 0 until vectorPeRow)
    io.outputC(a) := RegNext(vectorProcessingElementVector(a)(vectorPeCol - 1).io.outputC, ev.zero(outputTypeC.getWidth))

}

