package stag

import stag.common.SystolicTensorArrayConfig

case class AppConfig (
  splitVerilogOutput: Boolean,
  dataflow: Dataflow.Value,
  arrayConfig: SystolicTensorArrayConfig,
  integerType: IntegerType.Value,
  portBitWidthInfo: PortBitWidthInfo
)