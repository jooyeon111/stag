package stag.output

import chisel3._
import chisel3.util.{MuxCase, log2Ceil}
import stag.common.{Arithmetic, VerilogNaming}

import scala.math.{ceil, log10}

class Railway[T <: Data](
  groupPeRow: Int,
  groupPeCol: Int,
  vectorPeRow: Int,
  vectorPeCol: Int,
  portType: T
)(implicit ev: Arithmetic[T]) extends Module with VerilogNaming {

  override val desiredName:String = camelToSnake(this.getClass.getSimpleName)

  val numInput: Int = (groupPeRow + groupPeCol - 1) * vectorPeRow * vectorPeCol
  val muxSignalBit = log2Ceil(groupPeCol)
  val numOutput: Int = groupPeRow * vectorPeRow * vectorPeCol

  val io = IO(new Bundle {
    val input = Input(Vec(numInput, portType))
    val control = Input(UInt(muxSignalBit.W))
    val output = Output(Vec(numOutput, portType))
  })

  val semaphore = for ( i <- 0 until numOutput) yield {
    (0 until groupPeCol).map { j =>
      (io.control === j.U) -> io.input(i + (vectorPeRow * vectorPeCol * j))
    }
  }

  io.output.zipWithIndex.foreach { case (out, i) =>
    out := RegNext(MuxCase(ev.zero(portType.getWidth), semaphore(i)), ev.zero(portType.getWidth))
  }

}
