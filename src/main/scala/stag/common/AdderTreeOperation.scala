package stag.common

import chisel3._
import chisel3.util.log2Ceil

trait AdderTreeOperation[InputType <: Data, OutputType <: Data] {
  def add(input0: InputType, input1: InputType): InputType
  def getOutputType(input: Seq[InputType]): OutputType
  def zero(outputType: OutputType): OutputType
}

object AdderTreeOperation {

  implicit val sIntAdderTreeOperation: AdderTreeOperation[SInt, SInt] = new AdderTreeOperation[SInt, SInt] {
    override def add(input0: SInt, input1: SInt): SInt = input0 +& input1
    override def getOutputType(input: Seq[SInt]): SInt = SInt( (input.head.getWidth + log2Ceil(input.length)).W )
    override def zero(outputType: SInt): SInt = 0.S
  }

  implicit val uIntAdderTreeOperation: AdderTreeOperation[UInt, UInt] = new AdderTreeOperation[UInt, UInt] {
    override def add(input0: UInt, input1: UInt): UInt = input0 +& input1
    override def getOutputType(input: Seq[UInt]): UInt = UInt( (input.head.getWidth + log2Ceil(input.length)).W )
    override def zero(outputType: UInt): UInt = 0.U(outputType.getWidth.W)
  }

}