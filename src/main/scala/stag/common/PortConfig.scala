package stag.common

import chisel3._

class PortConfig[T <: Data](
  val inputTypeA: T,
  val inputTypeB: T,
  val multiplierOutputType: T,
  val adderTreeOutputTypeType: T,
  val enableUserBitWidth: Boolean,
  private val systolicTensorArrayOutputTypeC: T
){

  def getStaOutputTypeC: T = {
    systolicTensorArrayOutputTypeC
  }

  def calculateOutputTypeC(bitWidth: Int): T = {
    require(!enableUserBitWidth, "Cannot use this function")
    require(bitWidth >= 1, "Bit Width at at least 1")
    (inputTypeA match {
      case _: SInt => SInt(bitWidth.W)
      case _: UInt => UInt(bitWidth.W)
      case _ => throw new IllegalArgumentException(s"Unsupported type: ${inputTypeA.getClass}")
    }).asInstanceOf[T]
  }

}
