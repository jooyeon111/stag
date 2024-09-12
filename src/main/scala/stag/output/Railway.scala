//package stag.output
//
//import chisel3._
//import stag.common.PortBitWidth
//
//class Railway(groupPeRow: Int, groupPeCol: Int, vectorPeRow: Int, vectorPeCol: Int, portBitWidth: PortBitWidth) extends Module{
//
//  val numInput: Int = (groupPeRow + groupPeCol - 1) * vectorPeRow * vectorPeCol
//  val numOutput: Int = groupPeRow * vectorPeRow * vectorPeCol
//
//  val io = IO(new Bundle {
//    val input: Vec[SInt] = Input(Vec(numInput, SInt(portBitWidth.bitWidthC.W)))
//    val output: Vec[SInt] = Output(Vec(numOutput, SInt(portBitWidth.bitWidthC.W)))
//  })
//
//
//}
