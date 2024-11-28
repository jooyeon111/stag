package stag.output

import chisel3._
import stag.common.{ PortConfig, SystolicTensorArrayConfig, Arithmetic, VerilogNaming}

class SystolicTensorArray[T <: Data](
  groupPeRow: Int,
  groupPeCol : Int,
  vectorPeRow : Int,
  vectorPeCol : Int,
  numPeMultiplier : Int,
  portConfig: PortConfig[T],
  generateRtl: Boolean
)( implicit ev: Arithmetic[T] ) extends Module with VerilogNaming{

  override val desiredName:String = camelToSnake(this.getClass.getSimpleName)

  def this(arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig[T], generateRtl: Boolean)(implicit ev: Arithmetic[T]) = this(
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
  val numProcessingElemnt: Int = vectorPeRow * vectorPeCol
  val numOutput: Int = (groupPeCol + groupPeRow - 1) * numProcessingElemnt
  val outputTypeC = portConfig.getStaOutputTypeC
  val numPartialSumReset = groupPeRow + groupPeCol - 1
  val numPropagateOutput: Int = groupPeCol - 1

  val groupProcessingElementVector = Vector.tabulate(groupPeRow, groupPeCol) { (row, col) =>

    val isFirstRow = row == 0
    val isLastRow = row == groupPeRow - 1
    val isLastCol = col == groupPeCol - 1

    val withOutputA = !isLastCol
    val withOutputB = !isLastRow
    val withInputC = !isFirstRow && !isLastCol

    Module(new GroupProcessingElement(
      vectorPeRow = vectorPeRow,
      vectorPeCol = vectorPeCol,
      numPeMultiplier = numPeMultiplier,
      withOutputA = withOutputA,
      withOutputB = withOutputB,
      withInputC = withInputC,
      portConfig = portConfig
    ))
  }

  val io = IO(new Bundle {
    val inputA = Input(Vec(numInputA, portConfig.inputTypeA))
    val inputB = Input(Vec(numInputB, portConfig.inputTypeB))

    val partialSumReset = Input(Vec(numPartialSumReset, Bool()))
    val propagateOutput =  Input(Vec(numPropagateOutput, Bool()))

    val outputC = Output(Vec(numOutput, outputTypeC))
  })

  if(generateRtl){

    //Wiring Input A
    for( r <- 0 until groupPeRow )
      for( a <- 0 until vectorPeRow )
        for( p <- 0 until numPeMultiplier ) {
          val multiplierIndex = a * numPeMultiplier + p
          groupProcessingElementVector(r)(0).io.inputA(multiplierIndex) := RegNext( io.inputA(multiplierIndex + (r * vectorPeRow * numPeMultiplier)), ev.zero(portConfig.inputTypeA.getWidth))
        }

    //Wiring B
    for( c <- 0 until groupPeCol)
      for( b <- 0 until vectorPeCol)
        for (p <- 0 until numPeMultiplier) {
          val multiplierIndex = b * numPeMultiplier + p
          groupProcessingElementVector(0)(c).io.inputB(multiplierIndex) := RegNext( io.inputB(multiplierIndex + ( c * vectorPeCol * numPeMultiplier )), ev.zero(portConfig.inputTypeB.getWidth) )
        }

    for( r <- 1 until groupPeRow )
      for( c <- 0 until groupPeCol - 1 )
        groupProcessingElementVector(r)(c).io.partialSumReset := RegNext(io.partialSumReset(r + c), false.B)


    for (i <- 0 until numPropagateOutput)
      for (r <- 0 until groupPeRow)
        for (c <- 0 until groupPeCol)
          if (r - 1 == i && groupPeCol - 2 > c) {
            groupProcessingElementVector(r)(c).io.propagateOutput.get := RegNext(io.propagateOutput(i), false.B)
          } else if (r - 1 == i && groupPeCol - 2 - c == i) {
            groupProcessingElementVector(r)(c).io.propagateOutput.get := RegNext(io.propagateOutput(i), false.B)
          } else if (i < r - 1 && groupPeCol - 2 - c == i) {
            groupProcessingElementVector(r)(c).io.propagateOutput.get := RegNext(io.propagateOutput(i), false.B)
          }

  } else {

    //Wiring Input A
    for (r <- 0 until groupPeRow)
      for (a <- 0 until vectorPeRow)
        for (p <- 0 until numPeMultiplier) {
          val multiplierIndex = a * numPeMultiplier + p
          groupProcessingElementVector(r)(0).io.inputA(multiplierIndex) := io.inputA(multiplierIndex + (r * vectorPeRow * numPeMultiplier))
        }

    //Wiring B
    for (c <- 0 until groupPeCol)
      for (b <- 0 until vectorPeCol)
        for (p <- 0 until numPeMultiplier) {
          val multiplierIndex = b * numPeMultiplier + p
          groupProcessingElementVector(0)(c).io.inputB(multiplierIndex) := io.inputB(multiplierIndex + (c * vectorPeCol * numPeMultiplier))
        }


    for (r <- 0 until groupPeRow)
      for (c <- 0 until groupPeCol)
        groupProcessingElementVector(r)(c).io.partialSumReset := io.partialSumReset(r + c)

    for (i <- 0 until numPropagateOutput)
      for (r <- 0 until groupPeRow)
        for (c <- 0 until groupPeCol)
          if (r - 1 == i && groupPeCol - 2 > c) {
            groupProcessingElementVector(r)(c).io.propagateOutput.get := io.propagateOutput(i)
          } else if (r - 1 == i && groupPeCol - 2 - c == i) {
            groupProcessingElementVector(r)(c).io.propagateOutput.get := io.propagateOutput(i)
          } else if (i < r - 1 && groupPeCol - 2 - c == i) {
            groupProcessingElementVector(r)(c).io.propagateOutput.get := io.propagateOutput(i)
          }
  }

  //Wiring Input A
  for ( r <- 0 until groupPeRow )
    for ( c <- 1 until groupPeCol )
      for ( a <- 0 until vectorPeRow )
        for ( p <- 0 until numPeMultiplier ) {
          val multiplierIndex = a * numPeMultiplier + p
          groupProcessingElementVector(r)(c).io.inputA(multiplierIndex) := groupProcessingElementVector(r)(c-1).io.outputA.get(multiplierIndex)
        }

  for( r <- 1 until groupPeRow)
    for( c <- 0 until groupPeCol)
      for( b <- 0 until vectorPeCol)
        for( p <- 0 until numPeMultiplier) {
          val multiplierIndex = b * numPeMultiplier + p
          groupProcessingElementVector(r)(c).io.inputB(multiplierIndex) := groupProcessingElementVector(r-1)(c).io.outputB.get(multiplierIndex)
        }

  for(i <- 0 until groupPeRow; j <- 0 until groupPeCol; k <- 0 until numProcessingElemnt) {

    //Case0
    if(i == 0 && j == 0)
      io.outputC(k) := groupProcessingElementVector(i)(j).io.outputC(k)

    //Case1
    if( (0 < i && i < groupPeRow && j == 0 && i != 0) || ( i == groupPeRow - 1 && 0 < j &&  j < groupPeCol - 1 && i != 0) )
      io.outputC(i * numProcessingElemnt + j * numProcessingElemnt + k) := groupProcessingElementVector(i)(j).io.outputC(k)

    //Case2
    if( i == groupPeRow - 1 && j == groupPeCol - 1)
      io.outputC(i * numProcessingElemnt + j * numProcessingElemnt + k) := groupProcessingElementVector(i)(j).io.outputC(k)

    //Case3
    if( (0 <= i && i < groupPeRow - 1 && j == groupPeCol - 1 && j != 0) || (i == 0 && 0 < j && j < groupPeCol - 1 && j != 0 ))
      groupProcessingElementVector(i + 1)(j - 1).io.inputC.get(k) := groupProcessingElementVector(i)(j).io.outputC(k)

    //Case4
    if (0 < i  &&  i< groupPeRow - 1 && 0< j && j < groupPeCol - 1) {
      groupProcessingElementVector(i + 1)(j - 1).io.inputC.get(k) := groupProcessingElementVector(i)(j).io.outputC(k)

    }
  }

}