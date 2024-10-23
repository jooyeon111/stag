package stag.output

import chisel3._
import stag.common.{ PortConfig, SystolicTensorArrayConfig, Arithmetic}

class SystolicTensorArray[T <: Data](
  groupPeRow: Int,
  groupPeCol : Int,
  vectorPeRow : Int,
  vectorPeCol : Int,
  numPeMultiplier : Int,
  portConfig: PortConfig[T],
  generateRtl: Boolean
)( implicit ev: Arithmetic[T] ) extends Module{

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
  val numProcessingElemnt = vectorPeRow * vectorPeCol
  val numOutput: Int = (groupPeCol + groupPeRow - 1) * numProcessingElemnt
  val outputTypeC = portConfig.getStaOutputTypeC

  val groupProcessingElementVector = Vector.tabulate(groupPeRow, groupPeCol)( (row,col) => if( row == 0 ){
      if( col == groupPeCol - 1){
        Module(new GroupProcessingElement(vectorPeRow, vectorPeCol, numPeMultiplier, withOutputA = false, withOutputB = true, withInputC = false,  portConfig))
      } else {
        Module(new GroupProcessingElement(vectorPeRow, vectorPeCol, numPeMultiplier, withOutputA = true, withOutputB = true, withInputC = false,  portConfig))
      }

    } else if ( 0 < row && row < groupPeRow - 1){

      if( col == groupPeCol - 1){
        Module(new GroupProcessingElement(vectorPeRow, vectorPeCol, numPeMultiplier, withOutputA = false, withOutputB = true, withInputC = false,  portConfig))
      } else {
        Module(new GroupProcessingElement(vectorPeRow, vectorPeCol, numPeMultiplier, withOutputA = true, withOutputB = true, withInputC = true,  portConfig))
      }

    } else {
      if( col == groupPeCol - 1){
        Module(new GroupProcessingElement(vectorPeRow, vectorPeCol, numPeMultiplier, withOutputA = false, withOutputB = false, withInputC = false,  portConfig))
      } else {
        Module(new GroupProcessingElement(vectorPeRow, vectorPeCol, numPeMultiplier, withOutputA = true, withOutputB = false, withInputC = true,  portConfig))
      }
    })

  val io = IO(new Bundle {
    val inputA = Input(Vec(numInputA, portConfig.inputTypeA))
    val inputB = Input(Vec(numInputB, portConfig.inputTypeB))
    val propagateOutput =  Input(Vec(groupPeRow - 1, Vec(groupPeCol - 1, Bool())))
    val partialSumReset =  Input(Vec(groupPeRow, Vec(groupPeCol, Bool())))
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

    //Wiring propagate signal
    for( r <- 0 until groupPeRow - 1 )
      for( c<- 0 until groupPeCol - 1 )
        groupProcessingElementVector(r + 1)(c).io.propagateOutput.get := RegNext( io.propagateOutput(r)(c), false.B )

    //Wiring partial sum signals
    for( r <- 0 until groupPeRow )
      for( c <- 0 until groupPeCol )
        groupProcessingElementVector(r)(c).io.partialSumReset := RegNext( io.partialSumReset(r)(c), false.B )

  } else {

    //Wiring Input A
    for( r <- 0 until groupPeRow )
      for( a <- 0 until vectorPeRow )
        for( p <- 0 until numPeMultiplier ) {
          val multiplierIndex = a * numPeMultiplier + p
          groupProcessingElementVector(r)(0).io.inputA(multiplierIndex) := io.inputA(multiplierIndex + (r * vectorPeRow * numPeMultiplier))
        }

    //Wiring B
    for( c <- 0 until groupPeCol)
      for( b <- 0 until vectorPeCol)
        for (p <- 0 until numPeMultiplier) {
          val multiplierIndex = b * numPeMultiplier + p
          groupProcessingElementVector(0)(c).io.inputB(multiplierIndex) := io.inputB(multiplierIndex + ( c * vectorPeCol * numPeMultiplier ))
        }

    //Wiring propagate signal
    for( r <- 0 until groupPeRow - 1 )
      for( c<- 0 until groupPeCol - 1 )
        groupProcessingElementVector(r + 1)(c).io.propagateOutput.get := io.propagateOutput(r)(c)

    //Wiring partial sum signals
    for( r <- 0 until groupPeRow )
      for( c <- 0 until groupPeCol )
        groupProcessingElementVector(r)(c).io.partialSumReset := io.partialSumReset(r)(c)

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

  //TODO clean this code
  //Wiring Output
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
