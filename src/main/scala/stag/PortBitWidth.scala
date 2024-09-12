package stag

case class PortBitWidth(bitWidthA: Int, bitWidthB: Int, bitWidthC: Int){

  require(bitWidthA >= 1, "Bit Width A must be at least 1")
  require(bitWidthB >= 1, "Bit Width B must be at least 1")
  require(bitWidthC >= 1, "Bit Width C must be at least 1")

}