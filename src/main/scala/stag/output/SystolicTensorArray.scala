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

  val processingElementVector = Vector.tabulate(groupPeRow, groupPeCol) { (row, col) =>

    val isFirstRow = row == 0
    val isLastRow = row == groupPeRow - 1
    val isLastCol = col == groupPeCol - 1

    val withOutputA = !isLastCol
    val withOutputB = !isLastRow
    val withInputC = !isFirstRow && !isLastCol

    if(vectorPeRow == 1 && vectorPeCol == 1){
      Module(new VectorProcessingElement(
        numPeMultiplier = numPeMultiplier,
        withOutputA = withOutputA,
        withOutputB = withOutputB,
        withInputC = withInputC,
        portConfig = portConfig,
      ))
    } else {
      Module(new GroupProcessingElement(
        vectorPeRow = vectorPeRow,
        vectorPeCol = vectorPeCol,
        numPeMultiplier = numPeMultiplier,
        withOutputA = withOutputA,
        withOutputB = withOutputB,
        withInputC = withInputC,
        portConfig = portConfig
      ))
    }: ProcessingElementIo[T]

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
          processingElementVector(r)(0).io.inputA(multiplierIndex) := RegNext( io.inputA(multiplierIndex + (r * vectorPeRow * numPeMultiplier)), ev.zero(portConfig.inputTypeA.getWidth))
        }

    //Wiring B
    for( c <- 0 until groupPeCol)
      for( b <- 0 until vectorPeCol)
        for (p <- 0 until numPeMultiplier) {
          val multiplierIndex = b * numPeMultiplier + p
          processingElementVector(0)(c).io.inputB(multiplierIndex) := RegNext( io.inputB(multiplierIndex + ( c * vectorPeCol * numPeMultiplier )), ev.zero(portConfig.inputTypeB.getWidth) )
        }

    for( r <- 1 until groupPeRow )
      for( c <- 0 until groupPeCol - 1 )
        processingElementVector(r)(c).io.partialSumReset := RegNext(io.partialSumReset(r + c), false.B)


    for (i <- 0 until numPropagateOutput)
      for (r <- 0 until groupPeRow)
        for (c <- 0 until groupPeCol)
          if (r - 1 == i && groupPeCol - 2 > c) {
            processingElementVector(r)(c).io.propagateOutput.get := RegNext(io.propagateOutput(i), false.B)
          } else if (r - 1 == i && groupPeCol - 2 - c == i) {
            processingElementVector(r)(c).io.propagateOutput.get := RegNext(io.propagateOutput(i), false.B)
          } else if (i < r - 1 && groupPeCol - 2 - c == i) {
            processingElementVector(r)(c).io.propagateOutput.get := RegNext(io.propagateOutput(i), false.B)
          }

  } else {

    //Wiring Input A
    for (r <- 0 until groupPeRow)
      for (a <- 0 until vectorPeRow)
        for (p <- 0 until numPeMultiplier) {
          val multiplierIndex = a * numPeMultiplier + p
          processingElementVector(r)(0).io.inputA(multiplierIndex) := io.inputA(multiplierIndex + (r * vectorPeRow * numPeMultiplier))
        }

    //Wiring B
    for (c <- 0 until groupPeCol)
      for (b <- 0 until vectorPeCol)
        for (p <- 0 until numPeMultiplier) {
          val multiplierIndex = b * numPeMultiplier + p
          processingElementVector(0)(c).io.inputB(multiplierIndex) := io.inputB(multiplierIndex + (c * vectorPeCol * numPeMultiplier))
        }


    for (r <- 0 until groupPeRow)
      for (c <- 0 until groupPeCol)
        processingElementVector(r)(c).io.partialSumReset := io.partialSumReset(r + c)

    for (i <- 0 until numPropagateOutput)
      for (r <- 0 until groupPeRow)
        for (c <- 0 until groupPeCol)
          if (r - 1 == i && groupPeCol - 2 > c) {
            processingElementVector(r)(c).io.propagateOutput.get := io.propagateOutput(i)
          } else if (r - 1 == i && groupPeCol - 2 - c == i) {
            processingElementVector(r)(c).io.propagateOutput.get := io.propagateOutput(i)
          } else if (i < r - 1 && groupPeCol - 2 - c == i) {
            processingElementVector(r)(c).io.propagateOutput.get := io.propagateOutput(i)
          }
  }

  //Wiring Input A
  for ( r <- 0 until groupPeRow )
    for ( c <- 1 until groupPeCol )
      for ( a <- 0 until vectorPeRow )
        for ( p <- 0 until numPeMultiplier ) {
          val multiplierIndex = a * numPeMultiplier + p
          processingElementVector(r)(c).io.inputA(multiplierIndex) := processingElementVector(r)(c-1).io.outputA.get(multiplierIndex)
        }

  for( r <- 1 until groupPeRow)
    for( c <- 0 until groupPeCol)
      for( b <- 0 until vectorPeCol)
        for( p <- 0 until numPeMultiplier) {
          val multiplierIndex = b * numPeMultiplier + p
          processingElementVector(r)(c).io.inputB(multiplierIndex) := processingElementVector(r-1)(c).io.outputB.get(multiplierIndex)
        }

  for {
    r <- 0 until groupPeRow
    c <- 0 until groupPeCol
  } {

    if (isOutputPosition(r, c)) {
      val currentPe = processingElementVector(r)(c)

      currentPe.io.outputC match {
        case vec: Vec[_] =>
          for ( i <- 0 until numProcessingElemnt) {
            val outputIndex = r * numProcessingElemnt + c * numProcessingElemnt + i
            io.outputC(outputIndex) := vec(i)
          }

        case data: Data =>
          val outputIndex = r * numProcessingElemnt + c * numProcessingElemnt
          io.outputC(outputIndex) := data

        case _ =>
          throw new Exception("Wrong wiring")
      }

    }

    if (canConnectDiagonally(r, c)) {
      val targetPe = processingElementVector(r+1)(c-1)
      val currentPe = processingElementVector(r)(c)

      (targetPe.io.inputC, currentPe.io.outputC) match {

        case (Some(inputC: Vec[_]), outputC: Vec[_]) =>
          for ( i <- 0 until numProcessingElemnt){
            inputC(i) := outputC(i)
          }

        case (Some(inputC: Data), outputC: Data) =>
          inputC := outputC

        case _ =>
          throw new Exception("Wrong wiring")

      }
    }

  }

  def isOutputPosition(r: Int, c: Int): Boolean = {
    val isFirstElement = r == 0 && c == 0
    val isLeftOrBottomEdge = (0 < r && r < groupPeRow && c == 0) ||
      (r == groupPeRow - 1 && 0 < c && c < groupPeCol - 1)
    val isLastElement = r == groupPeRow - 1 && c == groupPeCol - 1

    isFirstElement || isLeftOrBottomEdge || isLastElement
  }

  def canConnectDiagonally(r: Int, c: Int): Boolean = {
    val isRightEdgeExceptLast = (0 <= r && r < groupPeRow - 1 && c == groupPeCol - 1)
    val isTopEdgeExceptFirst = (r == 0 && 0 < c && c < groupPeCol - 1)
    val isMiddle = (0 < r && r < groupPeRow - 1 && 0 < c && c < groupPeCol - 1)

    isRightEdgeExceptLast || isTopEdgeExceptFirst || isMiddle
  }

}