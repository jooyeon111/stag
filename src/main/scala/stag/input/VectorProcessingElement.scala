package stag.input

import chisel3._
import stag.common.PortBitWidth
import stag.common.Mac

class VectorProcessingElement(peMultiplierCount: Int, flagInputC: Boolean, portBitWidth: PortBitWidth) extends Module {

  val mac: Mac = Module(new Mac(peMultiplierCount, portBitWidth))

  val io =  IO(new Bundle {

    //Input
    val inputA: Vec[SInt] = Input(Vec(peMultiplierCount, SInt(portBitWidth.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(peMultiplierCount, SInt(portBitWidth.bitWidthB.W)))
    val inputC: Option[SInt] = if(flagInputC) Some(Input(SInt(portBitWidth.bitWidthC.W))) else None

    //Control
    val propagateA: Bool = Input(Bool())

    //Output
    val outputA: Vec[SInt] = Output(Vec(peMultiplierCount, SInt(portBitWidth.bitWidthA.W)))
    val outputC: SInt = Output(SInt(portBitWidth.bitWidthC.W))

  })

  io.outputA := RegNext(Mux(io.propagateA, io.inputA, io.outputA), VecInit.fill(peMultiplierCount)(0.S))

  mac.io.inputA := io.inputA
  mac.io.inputB := io.inputB

  if(flagInputC)
    io.outputC := RegNext(mac.io.output + io.inputC.get, 0.S(portBitWidth.bitWidthC.W))
  else
    io.outputC := RegNext(mac.io.output, 0.S(portBitWidth.bitWidthC.W))


}
