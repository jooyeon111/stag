package stag.input

import chisel3._

trait ProcessingElementIo [T <: Data] {

  type OutputType <: Data
  type PropagateType <: Data

  val io: Bundle {
    val inputA: Vec[T]
    val inputB: Vec[T]
    val inputC: Option[OutputType]
    val propagateA: PropagateType
    val outputA: Option[Vec[T]]
    val outputB: Option[Vec[T]]
    val outputC: OutputType
  }

}
