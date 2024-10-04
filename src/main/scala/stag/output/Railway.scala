//package stag.output
//
//import chisel3._
//import stag.common.Arithmetic
//
////TODO code after control logic
//class Railway[T <: Data](
//  groupPeRow: Int,
//  groupPeCol: Int,
//  vectorPeRow: Int,
//  vectorPeCol: Int,
//  portType: T
//)(implicit ev: Arithmetic[T]) extends Module{
//
//  val numInput: Int = (groupPeRow + groupPeCol - 1) * vectorPeRow * vectorPeCol
//  val numOutput: Int = groupPeRow * vectorPeRow * vectorPeCol
//
//  val io = IO(new Bundle {
//    val input: Vec[SInt] = Input(Vec(numInput, portType))
//    val output: Vec[SInt] = Output(Vec(numOutput, portType))
//  })
//
//
//}
