package stag.common

import chisel3._

case class PortConfig[T <: Data](
  inputTypeA: T,
  inputTypeB: T,
  multiplierOutputType: T,
  adderTreeOutputTypeType: T,
//  outputTypeC: T
){

  def createOutputTypeC(bitWidth: Int): T = {
    require(bitWidth >= 1, "Bit Width at at least 1")
    (inputTypeA match {
      case _: SInt => SInt(bitWidth.W)
      case _: UInt => UInt(bitWidth.W)
      case _ => throw new IllegalArgumentException(s"Unsupported type: ${inputTypeA.getClass}")
    }).asInstanceOf[T]
  }

}
