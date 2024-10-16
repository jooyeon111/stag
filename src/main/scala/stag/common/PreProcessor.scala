package stag.common

import chisel3._
import chisel3.util.ShiftRegister

class PreProcessor[T <: Data](
  arrayConfig: Int,
  blockConfig: Int,
  numPeMultiplier: Int,
  skewFlag: Boolean,
  portType: T
)( implicit ev: Arithmetic[T] ) extends Module {

  val numPort: Int = arrayConfig * blockConfig * numPeMultiplier

  val io = IO(new Bundle {
    val input= Input(Vec(numPort, portType))
    val output = Output(Vec(numPort, portType))
  })

  if(skewFlag){

    for (i <- 0 until arrayConfig)
      for (j <- 0 until blockConfig * numPeMultiplier) {
        val index = i * blockConfig * numPeMultiplier + j
        val depth = i + 1
        io.output(index) := ShiftRegister(io.input(index), depth, ev.zero(portType.getWidth), true.B)
      }

  } else {

    for( i <- 0 until numPort)
      io.output(i) := RegNext( io.input(i), ev.zero(portType.getWidth) )

  }


}
