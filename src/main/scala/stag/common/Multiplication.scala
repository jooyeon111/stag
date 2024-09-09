package stag.common

import chisel3._

trait Multiplication [A <: Data, B <: Data, P <: Data] {
  def multiply(a: A, b: B): P
  def getOutputType(a: A, b: B): P
  def zero(outputType: P): P
}

object Multiplication {

  implicit val sIntMultiplication: Multiplication[SInt, SInt, SInt] = new Multiplication[SInt, SInt, SInt] {
    override def multiply(a: SInt, b: SInt): SInt = ( a * b ).asTypeOf(getOutputType(a, b))
    override def getOutputType(a: SInt, b: SInt): SInt = SInt((a.getWidth + b.getWidth).W)
    override def zero(outputType: SInt): SInt = 0.S( outputType.getWidth.W )
  }

  implicit val uIntMultiplication: Multiplication[UInt, UInt, UInt] = new Multiplication[UInt, UInt, UInt] {
    override def multiply(a: UInt, b: UInt): UInt = ( a * b ).asTypeOf(getOutputType(a, b))
    override def getOutputType(a: UInt, b: UInt): UInt = UInt((a.getWidth + b.getWidth).W)
    override def zero(outputType: UInt): UInt = 0.U( outputType.getWidth.W )
  }

}

