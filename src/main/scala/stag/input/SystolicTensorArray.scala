package stag.input

import chisel3._
import stag.common.SystolicTensorArrayConfig
import stag.common.PortBitWidth

class SystolicTensorArray(val groupPeRow: Int, val groupPeCol : Int, val vectorPeRow : Int, val vectorPeCol : Int, val numPeMultiplier : Int, portBitWidth: PortBitWidth, generateRtl: Boolean) extends Module {

  def this(arrayConfig: SystolicTensorArrayConfig, portBitWidth: PortBitWidth, generateRtl: Boolean) =
    this(arrayConfig.groupPeRow, arrayConfig.groupPeCol, arrayConfig.vectorPeRow, arrayConfig.vectorPeCol, arrayConfig.numPeMultiplier, portBitWidth, generateRtl)

  val numInputA: Int = groupPeRow * vectorPeRow * numPeMultiplier
  val numInputB: Int = groupPeCol * vectorPeCol * numPeMultiplier
  val numPropagateA: Int = groupPeCol * vectorPeCol
  val numOutput : Int = groupPeRow * vectorPeRow

  val groupProcessingElementVector: Vector[Vector[GroupProcessingElement]] = Vector.tabulate(groupPeRow, groupPeCol)((_,y) => if ( y == 0 ) {
    Module(new GroupProcessingElement(vectorPeRow, vectorPeCol, numPeMultiplier, flagInputC = false, portBitWidth))
  } else{
    Module(new GroupProcessingElement(vectorPeRow, vectorPeCol, numPeMultiplier, flagInputC = true, portBitWidth))
  })

  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numInputA, SInt(portBitWidth.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB, SInt(portBitWidth.bitWidthB.W)))
    val propagateA : Vec[Bool] = Input(Vec(numPropagateA, Bool()))
    val outputC: Vec[SInt] = Output(Vec(numOutput, SInt(portBitWidth.bitWidthC.W)))
  })

  if(generateRtl){

    //Wiring Input A
    for( r <- 0 until groupPeRow)
      for( a <- 0 until vectorPeRow)
        for( p <- 0 until numPeMultiplier) {
          val multiplierIndex = a * numPeMultiplier + p
          groupProcessingElementVector(r)(0).io.inputA(multiplierIndex) := RegNext( io.inputA(multiplierIndex + (r * vectorPeRow * numPeMultiplier)), 0.S)
        }

    //Wiring Input B
    for( c <- 0 until groupPeCol)
      for( b <- 0 until vectorPeCol)
        for( p <- 0 until numPeMultiplier) {
          val multiplierIndex = b * numPeMultiplier + p
          groupProcessingElementVector(0)(c).io.inputB(multiplierIndex) := RegNext( io.inputB( multiplierIndex + (c * vectorPeCol * numPeMultiplier )), 0.S)
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
