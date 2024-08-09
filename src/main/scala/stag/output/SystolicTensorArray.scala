package stag.output

import chisel3._
import stag.sub.PortConfig
import stag.sub.SystolicTensorArrayConfig

class SystolicTensorArray(val arrayRow: Int, val arrayCol : Int, val blockRow : Int, val blockCol : Int, val numPeMultiplier : Int, portConfig: PortConfig) extends Module{

  def this(arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig) =
    this(arrayConfig.arrayRow, arrayConfig.arrayCol, arrayConfig.blockRow, arrayConfig.blockCol, arrayConfig.numPeMultiplier, portConfig)

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

    //Input
    val inputA: Vec[SInt] = Input(Vec(numInputA,SInt(portConfig.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(numInputB,SInt(portConfig.bitWidthB.W)))

    //Control
    val propagateOutput: Vec[Vec[Bool]] =  Input(Vec(arrayRow - 1, Vec(arrayCol - 1, Bool())))
    val partialSumReset: Vec[Vec[Bool]] =  Input(Vec(arrayRow, Vec(arrayCol, Bool())))

    //Output
    val outputC: Vec[SInt] = Output(Vec(numOutput,SInt(portConfig.bitWidthC.W)))

  })

  //Wiring Input
  //Wiring A
  for (i <- 0 until arrayRow)
    for(k <- 0 until blockRow * numPeMultiplier)
      blockProcessingElementVector(i)(0).io.inputA(k) := io.inputA(k + (i*blockRow*numPeMultiplier))


  for (i <- 0 until arrayRow)
    for(j <- 1 until arrayCol )
      for(k <- 0 until blockRow * numPeMultiplier)
        blockProcessingElementVector(i)(j).io.inputA(k) := blockProcessingElementVector(i)(j-1).io.outputA(k)

  //Wiring B
  for (i <- 0 until arrayCol)
    for(k <- 0 until blockCol * numPeMultiplier)
      blockProcessingElementVector(0)(i).io.inputB(k) := io.inputB(k + (i*blockCol*numPeMultiplier))

  for (i <- 1 until arrayRow)
    for(j <- 0 until arrayCol)
      for(k <- 0 until blockCol * numPeMultiplier)
        blockProcessingElementVector(i)(j).io.inputB(k) := blockProcessingElementVector(i-1)(j).io.outputB(k)


  //Wiring control signals

  //Wiring propagate signal
  for(i <- 0 until arrayRow - 1)
    for(j<- 0 until arrayCol - 1)
      blockProcessingElementVector(i + 1)(j).io.propagateOutput.get := io.propagateOutput(i)(j)

  //Wiring partial sum signals
  for (i <- 0 until arrayRow)
    for(j <- 0 until arrayCol)
      blockProcessingElementVector(i)(j).io.partialSumReset := io.partialSumReset(i)(j)


  //Wiring Output
  //Wiring OutputC
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
