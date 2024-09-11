package stag.common

import chisel3._

trait MultiplierOperation [InputTypeA <: Data, InputTypeB <: Data, OutputType <: Data] {
  def multiply(inputA: InputTypeA, inputB: InputTypeB): OutputType
  def getOutputType(inputA: InputTypeA, inputB: InputTypeB): OutputType
  def zero(output: OutputType): OutputType
}

object MultiplierOperation {

  implicit val sIntMultiplierOperation: MultiplierOperation[SInt, SInt, SInt] = new MultiplierOperation[SInt, SInt, SInt] {
    override def multiply(inputA: SInt, inputB: SInt): SInt = ( inputA * inputB ).asTypeOf(getOutputType(inputA, inputB))
    override def getOutputType(inputA: SInt, inputB: SInt): SInt = SInt((inputA.getWidth + inputB.getWidth).W)
    override def zero(outputType: SInt): SInt = 0.S( outputType.getWidth.W )
  }

  implicit val uIntMultiplierOperation: MultiplierOperation[UInt, UInt, UInt] = new MultiplierOperation[UInt, UInt, UInt] {
    override def multiply(inputA: UInt, inputB: UInt): UInt = ( inputA * inputB ).asTypeOf(getOutputType(inputA, inputB))
    override def getOutputType(inputA: UInt, inputB: UInt): UInt = UInt((inputA.getWidth + inputB.getWidth).W)
    override def zero(outputType: UInt): UInt = 0.U( outputType.getWidth.W )
  }

}

