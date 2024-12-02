package stag

case class PortBitWidthInfo(
  bitWidthPortA: Int,
  bitWidthPortB: Int,
  bitWidthMultiplierOutput: Int,
  bitWidthAdderTreeOutput: Int,
  enableUserBitWidth: Boolean,
  bitWidthPortC: Int
)