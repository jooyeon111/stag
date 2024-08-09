package stag.sub

case class SystolicTensorArrayConfig(arrayRow: Int, arrayCol: Int, blockRow: Int, blockCol: Int, numPeMultiplier: Int ) {

  require(arrayRow >= 1, "[error] Array row must be at least 1")
  require(arrayCol >= 1, "[error] Array col must be at least 1")
  require(blockRow >= 1, "[error] Block row must be at least 1")
  require(blockCol >= 1, "[error] Block col must be at least 1")
  require(numPeMultiplier >= 1, "[error] Total number of multipliers must be at least 1")

}
