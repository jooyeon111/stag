package stag.common

case class SystolicTensorArrayConfig(arrayRow: Int, arrayCol: Int, blockRow: Int, blockCol: Int, numPeMultiplier: Int ) {

  require(arrayRow >= 1, "Array row must be at least 1")
  require(arrayCol >= 1, "Array col must be at least 1")
  require(blockRow >= 1, "Block row must be at least 1")
  require(blockCol >= 1, "Block col must be at least 1")
  require(numPeMultiplier >= 1, "Total number of multipliers must be at least 1")

}
