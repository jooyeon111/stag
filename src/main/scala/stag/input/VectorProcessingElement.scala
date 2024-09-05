package stag.input

import chisel3._
import stag.common.PortConfig
import stag.common.Mac

class VectorProcessingElement(peMultiplierCount: Int, flagInputC: Boolean, portConfig: PortConfig) extends Module {

  val mac: Mac = Module(new Mac(peMultiplierCount, portConfig))

  val io =  IO(new Bundle {

    //Input
    val inputA: Vec[SInt] = Input(Vec(peMultiplierCount, SInt(portConfig.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(peMultiplierCount, SInt(portConfig.bitWidthB.W)))
    val inputC: Option[SInt] = if(flagInputC) Some(Input(SInt(portConfig.bitWidthC.W))) else None

    //Control
    val propagateA: Bool = Input(Bool())

    //Output
    val outputA: Vec[SInt] = Output(Vec(peMultiplierCount, SInt(portConfig.bitWidthA.W)))
    val outputC: SInt = Output(SInt(portConfig.bitWidthC.W))

  })

  io.outputA := RegNext(Mux(io.propagateA, io.inputA, io.outputA), VecInit.fill(peMultiplierCount)(0.S))

  mac.io.inputA := io.inputA
  mac.io.inputB := io.inputB

  if(flagInputC)
    io.outputC := RegNext(mac.io.output + io.inputC.get, 0.S(portConfig.bitWidthC.W))
  else
    io.outputC := RegNext(mac.io.output, 0.S(portConfig.bitWidthC.W))


}
