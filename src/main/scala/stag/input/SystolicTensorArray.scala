package stag.input

import chisel3._
import stag.common.{PortConfig, SystolicTensorArrayConfig, Arithmetic}

class SystolicTensorArray[T <: Data](
  groupPeRow: Int,
  groupPeCol : Int,
  vectorPeRow : Int,
  vectorPeCol : Int,
  numPeMultiplier : Int,
  portConfig: PortConfig[T],
  generateRtl: Boolean
)( implicit ev: Arithmetic[T] ) extends Module {

  def this(arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig[T], generateRtl: Boolean)(implicit ev: Arithmetic[T]) =
    this(arrayConfig.groupPeRow, arrayConfig.groupPeCol, arrayConfig.vectorPeRow, arrayConfig.vectorPeCol, arrayConfig.numPeMultiplier, portConfig, generateRtl)

  val numInputA: Int = groupPeRow * vectorPeRow * numPeMultiplier
  val numInputB: Int = groupPeCol * vectorPeCol * numPeMultiplier
  val numPropagateA: Int = groupPeCol * vectorPeCol
  val numOutput : Int = groupPeRow * vectorPeRow

  val outputTypeC = portConfig.getStaOutputTypeC

  val groupProcessingElementVector= Vector.tabulate(groupPeRow, groupPeCol)((_, groupPeColIndex) => if ( groupPeColIndex == 0 ) {
    Module(new GroupProcessingElement(groupPeColIndex, vectorPeRow, vectorPeCol, numPeMultiplier, flagInputC = false, portConfig))
  } else{
    Module(new GroupProcessingElement(groupPeColIndex, vectorPeRow, vectorPeCol, numPeMultiplier, flagInputC = true, portConfig))
  })

  val io = IO(new Bundle {
    val inputA = Input(Vec(numInputA, portConfig.inputTypeA))
    val inputB = Input(Vec(numInputB, portConfig.inputTypeB))
    val propagateA  = Input(Vec(numPropagateA, Bool()))
    val outputC = Output(Vec(numOutput, outputTypeC))
  })

  if(generateRtl){

    //Wiring Input A
    for( r <- 0 until groupPeRow)
      for( a <- 0 until vectorPeRow)
        for( p <- 0 until numPeMultiplier) {
          val multiplierIndex = a * numPeMultiplier + p
          groupProcessingElementVector(r)(0).io.inputA(multiplierIndex) := RegNext( io.inputA(multiplierIndex + (r * vectorPeRow * numPeMultiplier)), ev.zero(portConfig.inputTypeA.getWidth))
        }

    //Wiring Input B
    for( c <- 0 until groupPeCol)
      for( b <- 0 until vectorPeCol)
        for( p <- 0 until numPeMultiplier) {
          val multiplierIndex = b * numPeMultiplier + p
          groupProcessingElementVector(0)(c).io.inputB(multiplierIndex) := RegNext( io.inputB( multiplierIndex + (c * vectorPeCol * numPeMultiplier )), ev.zero(portConfig.inputTypeB.getWidth))
        }

    //Wiring Control
    for( r <- 0 until groupPeRow )
      for( c <- 0 until groupPeCol )
        for( b <- 0 until vectorPeCol)
          groupProcessingElementVector(r)(c).io.propagateA(b) := RegNext( io.propagateA(b + c * vectorPeCol), false.B)

  } else {

    //Wiring Input A
    for( r <- 0 until groupPeRow)
      for( a <- 0 until vectorPeRow)
        for( p <- 0 until numPeMultiplier) {
          val multiplierIndex = a * numPeMultiplier + p
          groupProcessingElementVector(r)(0).io.inputA(multiplierIndex) := io.inputA(multiplierIndex + (r * vectorPeRow * numPeMultiplier))
        }

    //Wiring Input B
    for( c <- 0 until groupPeCol)
      for( b <- 0 until vectorPeCol)
        for( p <- 0 until numPeMultiplier) {
          val multiplierIndex = b * numPeMultiplier + p
          groupProcessingElementVector(0)(c).io.inputB(multiplierIndex) := io.inputB( multiplierIndex + (c * vectorPeCol * numPeMultiplier ))
        }

    //Wiring Control
    for( r <- 0 until groupPeRow )
      for( c <- 0 until groupPeCol )
        for( b <- 0 until vectorPeCol)
          groupProcessingElementVector(r)(c).io.propagateA(b) := io.propagateA(b + c * vectorPeCol)

  }

  //Wiring Output A
  for( r <- 0 until groupPeRow)
    for( c <- 1 until groupPeCol)
      for( a <- 0 until vectorPeRow)
        for( p <- 0 until numPeMultiplier) {
          val multiplierIndex = a * numPeMultiplier + p
          groupProcessingElementVector(r)(c).io.inputA(multiplierIndex) := groupProcessingElementVector(r)(c - 1).io.outputA(multiplierIndex)
        }

  //Wiring Output B
  for( r <- 1 until groupPeRow )
    for( c <- 0 until groupPeCol )
      for( b <- 0 until vectorPeCol )
        for( p <- 0 until numPeMultiplier ) {
          val multiplierIndex = b * numPeMultiplier + p
          groupProcessingElementVector(r)(c).io.inputB(multiplierIndex) := groupProcessingElementVector(r-1)(c).io.outputB(multiplierIndex)
        }

  //Wiring Output
  for( r <- 0 until groupPeRow )
    for( c <- 1 until groupPeCol )
      for( a <- 0 until vectorPeRow )
        groupProcessingElementVector(r)(c).io.inputC.get(a) := groupProcessingElementVector(r)(c - 1).io.outputC(a)

  for( r <- 0 until groupPeRow )
    for( a <- 0 until vectorPeRow )
      io.outputC(a + (r * vectorPeRow)) := groupProcessingElementVector(r)(groupPeCol - 1).io.outputC(a)





}
