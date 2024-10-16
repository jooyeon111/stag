package stag.output

import chisel3._
import stag.common.Arithmetic
import scala.math.{ceil, log10}

class PostProcessor[ T <: Data](
  groupPeRow: Int,
  groupPeCol: Int,
  vectorPeRow: Int,
  vectorPeCol: Int,
  portType: T
)(implicit ev: Arithmetic[T]) extends Module{

  val numInput = (groupPeRow + groupPeCol - 1) * vectorPeRow * vectorPeCol
  val muxSignalBit = ceil(log10(groupPeCol.toDouble) /  log10(2.0)).toInt
  val numOutput = groupPeRow * vectorPeRow * vectorPeCol

  val deskewBuffer = Module(new DeskewBuffer(groupPeRow, groupPeCol, vectorPeRow, vectorPeCol, portType))
  val railway = Module(new Railway(groupPeRow, groupPeCol, vectorPeRow, vectorPeCol, portType))

  val io = IO(new Bundle {
    val input = Input(Vec(numInput, portType))
    val control = Input(UInt(muxSignalBit.W))
    val output = Output(Vec(numOutput, portType))
  })

  deskewBuffer.io.input := io.input
  railway.io.input := deskewBuffer.io.output
  railway.io.control := io.control
  io.output := railway.io.output

}
