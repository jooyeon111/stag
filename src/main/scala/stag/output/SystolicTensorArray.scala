package stag.output

import chisel3._
import stag.common.PortConfig
import stag.common.SystolicTensorArrayConfig

class SystolicTensorArray(val arrayRow: Int, val arrayCol : Int, val blockRow : Int, val blockCol : Int, val numPeMultiplier : Int, portConfig: PortConfig, generateRtl: Boolean ) extends Module{

  def this(arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig, generateRtl: Boolean) =
    this(arrayConfig.arrayRow, arrayConfig.arrayCol, arrayConfig.blockRow, arrayConfig.blockCol, arrayConfig.numPeMultiplier, portConfig, generateRtl)

  val numInputA: Int = arrayRow * blockRow * numPeMultiplier
  val numInputB: Int = arrayCol * blockCol * numPeMultiplier
  val numProcessingElemnt = blockRow * blockCol
  val numOutput: Int = (arrayCol + arrayRow - 1) * numProcessingElemnt

  val blockProcessingElementVector: Vector[Vector[BlockProcessingElement]] =
    Vector.tabulate(arrayRow, arrayCol)( (x,y) => if( x == 0 || y == arrayCol - 1){
      Module(new BlockProcessingElement(blockRow, blockCol, numPeMultiplier, flagInputC = false, portConfig))
    } else {
      Module(new BlockProcessingElement(blockRow, blockCol, numPeMultiplier, flagInputC = true, portConfig))
    })

  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numInputA,SInt(portConfig.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB,SInt(portConfig.bitWidthB.W)))
    val propagateOutput: Vec[Vec[Bool]] =  Input(Vec(arrayRow - 1, Vec(arrayCol - 1, Bool())))
    val partialSumReset: Vec[Vec[Bool]] =  Input(Vec(arrayRow, Vec(arrayCol, Bool())))
    val outputC: Vec[SInt] = Output(Vec(numOutput,SInt(portConfig.bitWidthC.W)))
  })

  if(generateRtl){

    //Wiring Input A
    for( r <- 0 until arrayRow )
      for( a <- 0 until blockRow )
        for( p <- 0 until numPeMultiplier ) {
          val multiplierIndex = a * numPeMultiplier + p
          blockProcessingElementVector(r)(0).io.inputA(multiplierIndex) := RegNext( io.inputA(multiplierIndex + (r * blockRow * numPeMultiplier)), 0.S )
        }

    //Wiring B
    for( c <- 0 until arrayCol)
      for( b <- 0 until blockCol)
        for (p <- 0 until numPeMultiplier) {
          val multiplierIndex = b * numPeMultiplier + p
          blockProcessingElementVector(0)(c).io.inputB(multiplierIndex) := RegNext( io.inputB(multiplierIndex + ( c * blockCol * numPeMultiplier )), 0.S )
        }

    //Wiring propagate signal
    for( r <- 0 until arrayRow - 1 )
      for( c<- 0 until arrayCol - 1 )
        blockProcessingElementVector(r + 1)(c).io.propagateOutput.get := RegNext( io.propagateOutput(r)(c), false.B )

    //Wiring partial sum signals
    for( r <- 0 until arrayRow )
      for( c <- 0 until arrayCol )
        blockProcessingElementVector(r)(c).io.partialSumReset := RegNext( io.partialSumReset(r)(c), false.B )

  } else {

    //Wiring Input A
    for( r <- 0 until arrayRow )
      for( a <- 0 until blockRow )
        for( p <- 0 until numPeMultiplier ) {
          val multiplierIndex = a * numPeMultiplier + p
          blockProcessingElementVector(r)(0).io.inputA(multiplierIndex) := io.inputA(multiplierIndex + (r * blockRow * numPeMultiplier))
        }

    //Wiring B
    for( c <- 0 until arrayCol)
      for( b <- 0 until blockCol)
        for (p <- 0 until numPeMultiplier) {
          val multiplierIndex = b * numPeMultiplier + p
          blockProcessingElementVector(0)(c).io.inputB(multiplierIndex) := io.inputB(multiplierIndex + ( c * blockCol * numPeMultiplier ))
        }

    //Wiring propagate signal
    for( r <- 0 until arrayRow - 1 )
      for( c<- 0 until arrayCol - 1 )
        blockProcessingElementVector(r + 1)(c).io.propagateOutput.get := io.propagateOutput(r)(c)

    //Wiring partial sum signals
    for( r <- 0 until arrayRow )
      for( c <- 0 until arrayCol )
        blockProcessingElementVector(r)(c).io.partialSumReset := io.partialSumReset(r)(c)

  }

  //Wiring Input A
  for ( r <- 0 until arrayRow )
    for ( c <- 1 until arrayCol )
      for ( a <- 0 until blockRow )
        for ( p <- 0 until numPeMultiplier ) {
          val multiplierIndex = a * numPeMultiplier + p
          blockProcessingElementVector(r)(c).io.inputA(multiplierIndex) := blockProcessingElementVector(r)(c-1).io.outputA(multiplierIndex)
        }

  for( r <- 1 until arrayRow)
    for( c <- 0 until arrayCol)
      for( b <- 0 until blockCol)
        for( p <- 0 until numPeMultiplier) {
          val multiplierIndex = b * numPeMultiplier + p
          blockProcessingElementVector(r)(c).io.inputB(multiplierIndex) := blockProcessingElementVector(r-1)(c).io.outputB(multiplierIndex)
        }

  //TODO clean this code
  //Wiring Output
  for(i <- 0 until arrayRow; j <- 0 until arrayCol; k <- 0 until numProcessingElemnt) {

    //Case0
    if(i == 0 && j == 0)
      io.outputC(k) := blockProcessingElementVector(i)(j).io.outputC(k)

    //Case1
    if( (0 < i && i < arrayRow && j == 0 && i != 0) || ( i == arrayRow - 1 && 0 < j &&  j < arrayCol - 1 && i != 0) )
      io.outputC(i * numProcessingElemnt + j * numProcessingElemnt + k) := blockProcessingElementVector(i)(j).io.outputC(k)

    //Case2
    if( i == arrayRow - 1 && j == arrayCol - 1)
      io.outputC(i * numProcessingElemnt + j * numProcessingElemnt + k) := blockProcessingElementVector(i)(j).io.outputC(k)

    //Case3
    if( (0 <= i && i < arrayRow - 1 && j == arrayCol - 1 && j != 0) || (i == 0 && 0 < j && j < arrayCol - 1 && j != 0 ))
      blockProcessingElementVector(i + 1)(j - 1).io.inputC.get(k) := blockProcessingElementVector(i)(j).io.outputC(k)

    //Case4
    if (0 < i  &&  i< arrayRow - 1 && 0< j && j < arrayCol - 1) {
      blockProcessingElementVector(i + 1)(j - 1).io.inputC.get(k) := blockProcessingElementVector(i)(j).io.outputC(k)

    }
  }


}
