package stag.weight

import chisel3._
import stag.common.{SystolicTensorArrayConfig, PortConfig, Arithmetic, VerilogNaming}


class SystolicTensorArray[T <: Data](
  groupPeRow: Int,
  groupPeCol : Int,
  vectorPeRow : Int,
  vectorPeCol : Int,
  numPeMultiplier : Int,
  portConfig: PortConfig[T],
  generateRtl: Boolean
)(implicit ev: Arithmetic[T]) extends Module with VerilogNaming{

  override val desiredName:String = camelToSnake(this.getClass.getSimpleName)

  def this(arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig[T], generateRtl: Boolean)(implicit ev: Arithmetic[T]) =
    this(
      arrayConfig.groupPeRow,
      arrayConfig.groupPeCol,
      arrayConfig.vectorPeRow,
      arrayConfig.vectorPeCol,
      arrayConfig.numPeMultiplier,
      portConfig,
      generateRtl
    )

  val numInputA: Int = groupPeRow * vectorPeRow * numPeMultiplier
  val numInputB: Int = groupPeCol * vectorPeCol * numPeMultiplier
  val numPropagateB: Int = groupPeRow * vectorPeRow
  val numOutput : Int = groupPeCol * vectorPeCol
  val outputTypeC = portConfig.getStaOutputTypeC

  val groupProcessingElementVector = Vector.tabulate(groupPeRow, groupPeCol){ (rowIndex, colIndex) =>

    val isFirstRow = rowIndex == 0
    val isLastRow = rowIndex == groupPeRow - 1
    val isLastCol = colIndex == groupPeCol - 1

    val withOutputA = !isLastCol
    val withOutputB = !isLastRow
    val withInputC = !isFirstRow

    Module(new GroupProcessingElement(
      groupPeRowIndex = rowIndex,
      vectorPeRow = vectorPeRow,
      vectorPeCol = vectorPeCol,
      numPeMultiplier = numPeMultiplier,
      withOutputA = withOutputA,
      withOutputB = withOutputB,
      withInputC = withInputC,
      portConfig = portConfig
    ))


  }

//  val groupProcessingElementVector = Vector.tabulate(groupPeRow, groupPeCol)( (groupPeRowIndex, groupPeColIndex) =>
//    if(groupPeColIndex < groupPeCol - 1){
//      if(groupPeRowIndex == 0){
//        Module(new GroupProcessingElement(
//          groupPeRowIndex,
//          vectorPeRow,
//          vectorPeCol,
//          numPeMultiplier,
//          withOutputA = true,
//          withOutputB = true,
//          withInputC = false,
//          portConfig
//        ))
//      } else if (groupPeRowIndex < groupPeRow -1){
//        Module(new GroupProcessingElement(
//          groupPeRowIndex,
//          vectorPeRow,
//          vectorPeCol,
//          numPeMultiplier,
//          withOutputA = true,
//          withOutputB = true,
//          withInputC = true,
//          portConfig
//        ))
//      } else {
//        Module(new GroupProcessingElement(
//          groupPeRowIndex,
//          vectorPeRow,
//          vectorPeCol,
//          numPeMultiplier,
//          withOutputA = true,
//          withOutputB = false,
//          withInputC = true,
//          portConfig
//        ))
//      }
//
//    } else {
//      if(groupPeRowIndex == 0){
//        Module(new GroupProcessingElement(
//          groupPeRowIndex,
//          vectorPeRow,
//          vectorPeCol,
//          numPeMultiplier,
//          withOutputA = false,
//          withOutputB = true,
//          withInputC = false,
//          portConfig
//        ))
//      } else if (groupPeRowIndex < groupPeRow -1){
//        Module(new GroupProcessingElement(
//          groupPeRowIndex,
//          vectorPeRow,
//          vectorPeCol,
//          numPeMultiplier,
//          withOutputA = false,
//          withOutputB = true,
//          withInputC = true,
//          portConfig
//        ))
//      } else {
//        Module(new GroupProcessingElement(
//          groupPeRowIndex,
//          vectorPeRow,
//          vectorPeCol,
//          numPeMultiplier,
//          withOutputA = false,
//          withOutputB = false,
//          withInputC = true,
//          portConfig
//        ))
//      }
//    }
//
//  )

  val io = IO(new Bundle {
    val inputA = Input(Vec(numInputA, portConfig.inputTypeA))
    val inputB = Input(Vec(numInputB, portConfig.inputTypeB))
    val propagateB = Input(Vec(numPropagateB, Bool()))
    val outputC = Output(Vec(numOutput, outputTypeC))
  })

  if(generateRtl){
    //Wiring Input A
    for( r <- 0 until groupPeRow)
      for( a <- 0 until vectorPeRow)
        for( p <- 0 until numPeMultiplier){
          val multiplierIndex = a * numPeMultiplier + p
          groupProcessingElementVector(r)(0).io.inputA(multiplierIndex) := RegNext(io.inputA(multiplierIndex + (r * vectorPeRow * numPeMultiplier)), ev.zero(portConfig.inputTypeA.getWidth))
        }

    //Wiring Input B
    for( c <- 0 until groupPeCol )
      for( b <- 0 until vectorPeCol)
        for( p <- 0 until numPeMultiplier){
          val multiplierIndex = b * numPeMultiplier + p
          groupProcessingElementVector(0)(c).io.inputB(multiplierIndex) := RegNext(io.inputB(multiplierIndex + (c * vectorPeCol * numPeMultiplier)), ev.zero(portConfig.inputTypeB.getWidth))

        }

    //Wiring Control
    for( r <- 0 until groupPeRow )
      for( c <- 0 until groupPeCol )
        for( a <- 0 until vectorPeRow )
          groupProcessingElementVector(r)(c).io.propagateB(a) := RegNext(io.propagateB(a + r * vectorPeRow), false.B)

  } else {
    //Wiring Input A
    for( r <- 0 until groupPeRow)
      for( a <- 0 until vectorPeRow)
        for( p <- 0 until numPeMultiplier){
          val multiplierIndex = a * numPeMultiplier + p
          groupProcessingElementVector(r)(0).io.inputA(multiplierIndex) := io.inputA(multiplierIndex + (r * vectorPeRow * numPeMultiplier))
        }

    //Wiring Input B
    for( c <- 0 until groupPeCol )
      for( b <- 0 until vectorPeCol)
        for( p <- 0 until numPeMultiplier){
          val multiplierIndex = b * numPeMultiplier + p
          groupProcessingElementVector(0)(c).io.inputB(multiplierIndex) := io.inputB(multiplierIndex + (c * vectorPeCol * numPeMultiplier))

        }

    //Wiring Control
    for( r <- 0 until groupPeRow )
      for( c <- 0 until groupPeCol )
        for( a <- 0 until vectorPeRow )
          groupProcessingElementVector(r)(c).io.propagateB(a) := io.propagateB(a + r * vectorPeRow)

  }

  for( r <- 0 until groupPeRow )
    for( c <- 1 until groupPeCol )
      for( a <- 0 until vectorPeRow )
        for( p <- 0 until numPeMultiplier ){
          val multiplierIndex = a * numPeMultiplier + p
          groupProcessingElementVector(r)(c).io.inputA(multiplierIndex) := groupProcessingElementVector(r)(c - 1).io.outputA.get(multiplierIndex)
        }

  for( r <- 1 until groupPeRow )
    for( c <- 0 until groupPeCol )
      for( b <- 0 until vectorPeCol )
        for( p <- 0 until numPeMultiplier ){
          val multiplierIndex = b * numPeMultiplier + p
          groupProcessingElementVector(r)(c).io.inputB(multiplierIndex) := groupProcessingElementVector(r - 1)(c).io.outputB.get(multiplierIndex)
        }

  for( r <- 1 until groupPeRow )
    for( c <- 0 until groupPeCol )
      for( b <- 0 until vectorPeCol )
        groupProcessingElementVector(r)(c).io.inputC.get(b) := groupProcessingElementVector(r - 1)(c).io.outputC(b)

  for( c <- 0 until groupPeCol )
    for( b <- 0 until vectorPeCol )
      io.outputC(b + (c * vectorPeCol)) := groupProcessingElementVector(groupPeRow - 1)(c).io.outputC(b)

}
