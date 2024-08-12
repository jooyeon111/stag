package stag.weight

import chisel3._
import stag.common.SystolicTensorArrayConfig
import stag.common.PortConfig

class SystolicTensorArray(val arrayRow: Int, val arrayCol : Int, val blockRow : Int, val blockCol : Int, val numPeMultiplier : Int, portConfig: PortConfig, generateRtl: Boolean) extends Module{

  def this(arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig, generateRtl: Boolean) =
    this(arrayConfig.arrayRow, arrayConfig.arrayCol, arrayConfig.blockRow, arrayConfig.blockCol, arrayConfig.numPeMultiplier, portConfig, generateRtl)

  val numInputA: Int = arrayRow * blockRow * numPeMultiplier
  val numInputB: Int = arrayCol * blockCol * numPeMultiplier
  val numOutput : Int = arrayCol * blockCol

  val blockProcessingElementVector: Vector[Vector[BlockProcessingElement]] = Vector.tabulate(arrayRow, arrayCol)((x,_) => if ( x == 0 ) {
    Module(new BlockProcessingElement(blockRow, blockCol, numPeMultiplier, flagInputC = false, portConfig))

  } else{
    Module(new BlockProcessingElement(blockRow, blockCol, numPeMultiplier, flagInputC = true, portConfig))
  })

  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(numInputA, SInt(portConfig.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB, SInt(portConfig.bitWidthB.W)))
    val propagateB : Vec[Bool] = Input(Vec(arrayRow, Bool()))
    val outputC: Vec[SInt] = Output(Vec(numOutput, SInt(portConfig.bitWidthC.W)))
  })

  if(generateRtl){
    //Wiring Input A
    for( r <- 0 until arrayRow)
      for( a <- 0 until blockRow)
        for( p <- 0 until numPeMultiplier){
          val multiplierIndex = a * numPeMultiplier + p
          blockProcessingElementVector(r)(0).io.inputA(multiplierIndex) := RegNext(io.inputA(multiplierIndex + (r * blockRow * numPeMultiplier)), 0.S)
        }

    //Wiring Input B
    for( c <- 0 until arrayCol )
      for( b <- 0 until blockCol)
        for( p <- 0 until numPeMultiplier){
          val multiplierIndex = b * numPeMultiplier + p
          blockProcessingElementVector(0)(c).io.inputB(multiplierIndex) := RegNext(io.inputB(multiplierIndex + (c * blockCol * numPeMultiplier)), 0.S)

        }

    //Wiring Control
    for( r <- 0 until arrayRow )
      for( c <- 0 until arrayCol )
        blockProcessingElementVector(r)(c).io.propagateB := RegNext(io.propagateB(r), false.B)

  } else {
    //Wiring Input A
    for( r <- 0 until arrayRow)
      for( a <- 0 until blockRow)
        for( p <- 0 until numPeMultiplier){
          val multiplierIndex = a * numPeMultiplier + p
          blockProcessingElementVector(r)(0).io.inputA(multiplierIndex) := io.inputA(multiplierIndex + (r * blockRow * numPeMultiplier))
        }

    //Wiring Input B
    for( c <- 0 until arrayCol )
      for( b <- 0 until blockCol)
        for( p <- 0 until numPeMultiplier){
          val multiplierIndex = b * numPeMultiplier + p
          blockProcessingElementVector(0)(c).io.inputB(multiplierIndex) := io.inputB(multiplierIndex + (c * blockCol * numPeMultiplier))

        }

    //Wiring Control
    for( r <- 0 until arrayRow )
      for( c <- 0 until arrayCol )
        blockProcessingElementVector(r)(c).io.propagateB := io.propagateB(r)

  }

  for( r <- 0 until arrayRow )
    for( c <- 1 until arrayCol )
      for( a <- 0 until blockRow )
        for( p <- 0 until numPeMultiplier ){
          val multiplierIndex = a * numPeMultiplier + p
          blockProcessingElementVector(r)(c).io.inputA(multiplierIndex) := blockProcessingElementVector(r)(c - 1).io.outputA(multiplierIndex)
        }

  for( r <- 1 until arrayRow )
    for( c <- 0 until arrayCol )
      for( b <- 0 until blockCol )
        for( p <- 0 until numPeMultiplier ){
          val multiplierIndex = b * numPeMultiplier + p
          blockProcessingElementVector(r)(c).io.inputB(multiplierIndex) := blockProcessingElementVector(r - 1)(c).io.outputB(multiplierIndex)
        }

  for( r <- 1 until arrayRow )
    for( c <- 0 until arrayCol )
      for( b <- 0 until blockCol )
        blockProcessingElementVector(r)(c).io.inputC.get(b) := blockProcessingElementVector(r - 1)(c).io.outputC(b)

  for( c <- 0 until arrayCol )
    for( b <- 0 until blockCol )
      io.outputC(b + (c * blockCol)) := blockProcessingElementVector(arrayRow - 1)(c).io.outputC(b)

}
