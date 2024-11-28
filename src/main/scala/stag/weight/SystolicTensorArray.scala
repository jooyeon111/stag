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
  val outputTypeC: T = portConfig.getStaOutputTypeC

  val processingElementVector = Vector.tabulate(groupPeRow, groupPeCol){ (rowIndex, colIndex) =>

    val isFirstRow = rowIndex == 0
    val isLastRow = rowIndex == groupPeRow - 1
    val isLastCol = colIndex == groupPeCol - 1

    val withOutputA = !isLastCol
    val withOutputB = !isLastRow
    val withInputC = !isFirstRow

    if(vectorPeRow == 1 && vectorPeCol == 1){

      Module(new VectorProcessingElement(
        groupPeRowIndex = 0,
        vectorPeRowIndex = rowIndex,
        vectorPeRow = 0,
        numPeMultiplier = numPeMultiplier,
        withOutputA = withOutputA,
        withOutputB = withOutputB,
        withInputC = withInputC,
        portConfig = portConfig
      ))

    } else {

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

    }: ProcessingElementIo[T]

  }

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
          processingElementVector(r)(0).io.inputA(multiplierIndex) := RegNext(io.inputA(multiplierIndex + (r * vectorPeRow * numPeMultiplier)), ev.zero(portConfig.inputTypeA.getWidth))
        }

    //Wiring Input B
    for( c <- 0 until groupPeCol )
      for( b <- 0 until vectorPeCol)
        for( p <- 0 until numPeMultiplier){
          val multiplierIndex = b * numPeMultiplier + p
          processingElementVector(0)(c).io.inputB(multiplierIndex) := RegNext(io.inputB(multiplierIndex + (c * vectorPeCol * numPeMultiplier)), ev.zero(portConfig.inputTypeB.getWidth))

        }

    //Wiring Control
//    for( r <- 0 until groupPeRow )
//      for( c <- 0 until groupPeCol )
//        for( a <- 0 until vectorPeRow )
//          processingElementVector(r)(c).io.propagateB(a) := RegNext(io.propagateB(a + r * vectorPeRow), false.B)

    for( r <- 0 until groupPeRow )
      for( c <- 0 until groupPeCol ) {
        val pe = processingElementVector(r)(c)
        pe.io.propagateB match {
          case vec: Vec[Bool] =>
            for( a <- 0 until vectorPeRow)
              vec(a) := RegNext(io.propagateB(a + r * vectorPeRow), false.B)

          case bool: Bool =>
            bool := RegNext(io.propagateB(r), false.B)
        }
      }


  } else {
    //Wiring Input A
    for( r <- 0 until groupPeRow)
      for( a <- 0 until vectorPeRow)
        for( p <- 0 until numPeMultiplier){
          val multiplierIndex = a * numPeMultiplier + p
          processingElementVector(r)(0).io.inputA(multiplierIndex) := io.inputA(multiplierIndex + (r * vectorPeRow * numPeMultiplier))
        }

    //Wiring Input B
    for( c <- 0 until groupPeCol )
      for( b <- 0 until vectorPeCol)
        for( p <- 0 until numPeMultiplier){
          val multiplierIndex = b * numPeMultiplier + p
          processingElementVector(0)(c).io.inputB(multiplierIndex) := io.inputB(multiplierIndex + (c * vectorPeCol * numPeMultiplier))

        }

    //Wiring Control
//    for( r <- 0 until groupPeRow )
//      for( c <- 0 until groupPeCol )
//        for( a <- 0 until vectorPeRow )
//          processingElementVector(r)(c).io.propagateB(a) := io.propagateB(a + r * vectorPeRow)


    for( r <- 0 until groupPeRow )
      for( c <- 0 until groupPeCol ) {
        val pe = processingElementVector(r)(c)
        pe.io.propagateB match {
          case vec: Vec[Bool] =>
            for( a <- 0 until vectorPeRow)
              vec(a) := RegNext(io.propagateB(a + r * vectorPeRow), false.B)

          case bool: Bool =>
            bool := RegNext(io.propagateB(r), false.B)
        }
      }

  }

  for( r <- 0 until groupPeRow )
    for( c <- 1 until groupPeCol )
      for( a <- 0 until vectorPeRow )
        for( p <- 0 until numPeMultiplier ){
          val multiplierIndex = a * numPeMultiplier + p
          processingElementVector(r)(c).io.inputA(multiplierIndex) := processingElementVector(r)(c - 1).io.outputA.get(multiplierIndex)
        }

  for( r <- 1 until groupPeRow )
    for( c <- 0 until groupPeCol )
      for( b <- 0 until vectorPeCol )
        for( p <- 0 until numPeMultiplier ){
          val multiplierIndex = b * numPeMultiplier + p
          processingElementVector(r)(c).io.inputB(multiplierIndex) := processingElementVector(r - 1)(c).io.outputB.get(multiplierIndex)
        }

//  for( r <- 1 until groupPeRow )
//    for( c <- 0 until groupPeCol )
//      for( b <- 0 until vectorPeCol )
//        processingElementVector(r)(c).io.inputC.get(b) := processingElementVector(r - 1)(c).io.outputC(b)

  for( r <- 1 until groupPeRow )
    for( c <- 0 until groupPeCol )
      (processingElementVector(r)(c).io.inputC, processingElementVector(r-1)(c).io.outputC) match {
        case (Some(inputC: Vec[_]), outputC: Vec[_]) =>
          for( b <- 0 until vectorPeCol)
            inputC(b) := outputC(b)

        case (Some(inputC: Data), outputC: Data) =>
          inputC := outputC
        case _ =>
          throw new Exception("Wrong Type")
      }

//  for( c <- 0 until groupPeCol )
//    for( b <- 0 until vectorPeCol )
//      io.outputC(b + (c * vectorPeCol)) := processingElementVector(groupPeRow - 1)(c).io.outputC(b)


  for ( c <- 0 until groupPeCol){
    val lastPe = processingElementVector(groupPeRow - 1)(c)
    lastPe.io.outputC match {
      case vec: Vec[_] =>
        for( b <- 0 until vectorPeCol)
          io.outputC(b+(c*vectorPeCol)) := vec(b)

      case data: Data =>
        io.outputC(c) := data


      case _ =>
        throw new Exception("Wrong Type")
    }
  }

}
