package stag.common

import chisel3._

case class PortConfig[T <: Data](
  inputTypeA: T,
  inputTypeB: T,
  multiplierOutputType: T,
  adderTreeOutputTypeType: T,
  outputTypeC: T
){

}
