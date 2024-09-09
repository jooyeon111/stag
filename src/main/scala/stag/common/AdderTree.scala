package stag.common

import chisel3._

class AdderTree(numPeMultiplier: Int, portBitWidth: PortBitWidth) extends Module{

  val io = IO(new Bundle {
    val input: Vec[SInt] = Input(Vec(numPeMultiplier, SInt((portBitWidth.bitWidthA + portBitWidth.bitWidthB).W)))
    val output: SInt = Output(SInt(portBitWidth.bitWidthC.W))
  })

  if(numPeMultiplier == 1){
    io.output := io.input(0)
  } else {
    io.output := io.input.reduceTree(
      (a,b) => RegNext(a +& b, 0.S),
      a => RegNext(a, 0.S)
    )
  }

}