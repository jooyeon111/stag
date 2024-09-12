package stag.common

import chisel3._

trait Arithmetic[T <: Data] {

  def zero(width: Int): T
  def add(input0: T, input1: T) : T
  def multiply(inputA: T, inputB : T): T

}


object Arithmetic {

  implicit val sIntArithmetic: Arithmetic[SInt] = new Arithmetic[SInt] {
    override def zero(width: Int): SInt =  0.S(width.W)
    override def add(input0: SInt, input1: SInt): SInt = input0 +& input1
    override def multiply(inputA: SInt, inputB : SInt) : SInt = inputA * inputB
  }

  implicit val uIntArithmetic: Arithmetic[UInt] = new Arithmetic[UInt] {
    override def zero(width: Int): UInt =  0.U(width.W)
    override def add(input0: UInt, input1: UInt): UInt = input0 +& input1
    override def multiply(inputA: UInt, inputB : UInt) : UInt = inputA * inputB
  }

}