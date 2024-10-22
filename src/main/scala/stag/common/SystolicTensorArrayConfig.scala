package stag.common

case class SystolicTensorArrayConfig(groupPeRow: Int, groupPeCol: Int, vectorPeRow: Int, vectorPeCol: Int, numPeMultiplier: Int ) {

  val arrayConfigString = s"${groupPeRow}x${groupPeCol}x${vectorPeRow}x${vectorPeCol}x$numPeMultiplier"

  require(groupPeRow >= 1, "Array row must be at least 1")
  require(groupPeCol >= 1, "Array col must be at least 1")
  require(vectorPeRow >= 1, "Block row must be at least 1")
  require(vectorPeCol >= 1, "Block col must be at least 1")
  require(numPeMultiplier >= 1, "Total number of multipliers must be at least 1")

}
