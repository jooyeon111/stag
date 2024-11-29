package stag.output

import chisel3._

trait ProcessingElementIo[T<: Data] {

  type OutputType <: Data

  val io: Bundle {
    val inputA: Vec[T]
    val inputB: Vec[T]
    val inputC: Option[OutputType]
    val partialSumReset: Bool
    val propagateOutput: Option[Bool]
    val outputA: Option[Vec[T]]
    val outputB: Option[Vec[T]]
    val outputC: OutputType
  }

}
