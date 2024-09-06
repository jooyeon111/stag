package stag.output

import chisel3._
import stag.common.PortBitWidth
import stag.common.SystolicTensorArrayConfig

class SystolicTensorArray(val groupPeRow: Int, val groupPeCol : Int, val vectorPeRow : Int, val vectorPeCol : Int, val numPeMultiplier : Int, portBitWidth: PortBitWidth, generateRtl: Boolean ) extends Module{

  def this(arrayConfig: SystolicTensorArrayConfig, portBitWidth: PortBitWidth, generateRtl: Boolean) =
    this(arrayConfig.groupPeRow, arrayConfig.groupPeCol, arrayConfig.vectorPeRow, arrayConfig.vectorPeCol, arrayConfig.numPeMultiplier, portBitWidth, generateRtl)

  val numInputA: Int = groupPeRow * vectorPeRow * numPeMultiplier
  val numInputB: Int = groupPeCol * vectorPeCol * numPeMultiplier
  val numProcessingElemnt = vectorPeRow * vectorPeCol
  val numOutput: Int = (groupPeCol + groupPeRow - 1) * numProcessingElemnt

  val groupProcessingElementVector: Vector[Vector[GroupProcessingElement]] =
    Vector.tabulate(groupPeRow, groupPeCol)( (x,y) => if( x == 0 || y == groupPeCol - 1){
      Module(new GroupProcessingElement(vectorPeRow, vectorPeCol, numPeMultiplier, flagInputC = false, portBitWidth))
    } else {
      Module(new GroupProcessingElement(vectorPeRow, vectorPeCol, numPeMultiplier, flagInputC = true, portBitWidth))
    })

  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numInputA,SInt(portBitWidth.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB,SInt(portBitWidth.bitWidthB.W)))
    val propagateOutput: Vec[Vec[Bool]] =  Input(Vec(groupPeRow - 1, Vec(groupPeCol - 1, Bool())))
    val partialSumReset: Vec[Vec[Bool]] =  Input(Vec(groupPeRow, Vec(groupPeCol, Bool())))
    val outputC: Vec[SInt] = Output(Vec(numOutput,SInt(portBitWidth.bitWidthC.W)))
  })

  if(generateRtl){

    //Wiring Input A
    for( r <- 0 until groupPeRow )
      for( a <- 0 until vectorPeRow )
        for( p <- 0 until numPeMultiplier ) {
          val multiplierIndex = a * numPeMultiplier + p
          groupProcessingElementVector(r)(0).io.inputA(multiplierIndex) := RegNext( io.inputA(multiplierIndex + (r * vectorPeRow * numPeMultiplier)), 0.S )
        }

    //Wiring B
    for( c <- 0 until groupPeCol)
      for( b <- 0 until vectorPeCol)
        for (p <- 0 until numPeMultiplier) {
          val multiplierIndex = b * numPeMultiplier + p
          groupProcessingElementVector(0)(c).io.inputB(multiplierIndex) := RegNext( io.inputB(multiplierIndex + ( c * vectorPeCol * numPeMultiplier )), 0.S )
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
          groupProcessingElementVector(r)(c).io.inputA(multiplierIndex) := groupProcessingElementVector(r)(c-1).io.outputA(multiplierIndex)
        }

  for( r <- 1 until groupPeRow)
    for( c <- 0 until groupPeCol)
      for( b <- 0 until vectorPeCol)
        for( p <- 0 until numPeMultiplier) {
          val multiplierIndex = b * numPeMultiplier + p
          groupProcessingElementVector(r)(c).io.inputB(multiplierIndex) := groupProcessingElementVector(r-1)(c).io.outputB(multiplierIndex)
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
