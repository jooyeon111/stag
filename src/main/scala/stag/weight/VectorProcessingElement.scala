package stag.weight

import chisel3._
import stag.common.PortBitWidth
import stag.common.Mac

class VectorProcessingElement(peMultiplierCount: Int, flagInputC: Boolean, portBitWidth: PortBitWidth) extends Module {

  //TODO fix it later just temporal code to prevent an error
  val mac: Mac[SInt, SInt, SInt, SInt] = Module(new Mac(peMultiplierCount, SInt(8.W), SInt(8.W)))

  val io =  IO(new Bundle {

    //Input
    val inputA: Vec[SInt] = Input(Vec(peMultiplierCount, SInt(portBitWidth.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(peMultiplierCount, SInt(portBitWidth.bitWidthB.W)))
    val inputC: Option[SInt] = if(flagInputC) Some(Input(SInt(portBitWidth.bitWidthC.W))) else None

    //Control
    val propagateB: Bool = Input(Bool())

    //Output
    val outputB: Vec[SInt] = Output(Vec(peMultiplierCount, SInt(portBitWidth.bitWidthB.W)))
    val outputC: SInt = Output(SInt(portBitWidth.bitWidthC.W))

  })

  io.outputB := RegNext(Mux(io.propagateB, io.inputB, io.outputB), VecInit.fill(peMultiplierCount)(0.S))

  mac.io.inputA := io.inputA
  mac.io.inputB := io.inputB

  if(flagInputC)
    io.outputC := RegNext(mac.io.output + io.inputC.get, 0.S(portBitWidth.bitWidthC.W))
  else
    io.outputC := RegNext(mac.io.output, 0.S(portBitWidth.bitWidthC.W))


}
