package stag.output

import chisel3._
import stag.common.PortConfig
import stag.common.Mac

class VectorProcessingElement(peMultiplierCount: Int, portConfig: PortConfig) extends Module {

  val mac: Mac = Module(new Mac(peMultiplierCount, portConfig))
  val outputRegister = RegInit(0.S(portConfig.bitWidthC.W))

  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(peMultiplierCount, SInt(portConfig.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(peMultiplierCount, SInt(portConfig.bitWidthB.W)))
    val partialSumReset: Bool = Input(Bool())
    val output: SInt = Output(SInt(portConfig.bitWidthC.W))
  })

  //Wiring input
  mac.io.inputA := io.inputA
  mac.io.inputB := io.inputB

  //Wiring control and output
  outputRegister := mac.io.output + Mux(io.partialSumReset, 0.S, outputRegister)
  io.output := outputRegister

}
