package stag.output

import chisel3._
import stag.common.PortBitWidth
import stag.common.Mac

class VectorProcessingElement(peMultiplierCount: Int, portBitWidth: PortBitWidth) extends Module {

  val mac: Mac = Module(new Mac(peMultiplierCount, portBitWidth))
  val outputRegister = RegInit(0.S(portBitWidth.bitWidthC.W))

  val io = IO(new Bundle {
    val inputA: Vec[SInt] = Input(Vec(peMultiplierCount, SInt(portBitWidth.bitWidthA.W)))
    val inputB: Vec[SInt] = Input(Vec(peMultiplierCount, SInt(portBitWidth.bitWidthB.W)))
    val partialSumReset: Bool = Input(Bool())
    val output: SInt = Output(SInt(portBitWidth.bitWidthC.W))
  })

  //Wiring input
  mac.io.inputA := io.inputA
  mac.io.inputB := io.inputB

  //Wiring control and output
  outputRegister := mac.io.output + Mux(io.partialSumReset, 0.S, outputRegister)
  io.output := outputRegister

}
