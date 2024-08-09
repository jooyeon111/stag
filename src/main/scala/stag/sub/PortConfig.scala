package stag.sub

case class PortConfig(bitWidthA: Int, bitWidthB: Int, bitWidthC: Int){

  require(bitWidthA >= 1, "[error] Bit Width A must be at least 1")
  require(bitWidthB >= 1, "[error] Bit Width B must be at least 1")
  require(bitWidthC >= 1, "[error] Bit Width C must be at least 1")
  
}
