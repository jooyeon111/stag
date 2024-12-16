package stag

import chisel3.util.log2Ceil
import stag.common.SystolicTensorArrayConfig

import scala.util.Try

trait ConfigurationParser {

  def parseArgs(args: Array[String]): Either[String, String] = {
    args match {
      case Array(fileName) => Right(fileName)
      case Array() => Left("No argument is provided")
      case _ => Left("Too many arguments are provided")
    }
  }

  def parseConfig(config: ConfigParser.Config): Try[AppConfig] = Try {

    val splitVerilogOutput = config.getBoolean("Split Verilog Output").get

    val dataflow = config.getString("Dataflow").get match {
      case "IS" => Dataflow.Is
      case "OS" => Dataflow.Os
      case "WS" => Dataflow.Ws
      case _ => throw new IllegalArgumentException("Invalid dataflow")
    }

    val arrayConfig = SystolicTensorArrayConfig(
      config.getInt("R").get,
      config.getInt("C").get,
      config.getInt("A").get,
      config.getInt("B").get,
      config.getInt("P").get,
    )

    val integerType = config.getString("Port Type").get match {
      case "Unsigned" => IntegerType.UnSigned
      case "Signed" => IntegerType.Signed
      case _ =>
        Console.err.println("Invalid integer type")
        sys.exit(1)
    }

    val bitWidthPortA = config.getInt("Port A").get
    val bitWidthPortB = config.getInt("Port B").get
    val bitWidthMultiplierOutput = bitWidthPortA + bitWidthPortB
    val bitWidthAdderTreeOutput = bitWidthMultiplierOutput + log2Ceil(arrayConfig.numPeMultiplier)
    var enableUserBitWidth = true

    val bitWidthPortC = config.getInt("Port C").getOrElse{

      enableUserBitWidth = false

      dataflow match {
        case Dataflow.Is =>
          bitWidthAdderTreeOutput + log2Ceil(arrayConfig.groupPeCol * arrayConfig.vectorPeCol)

        case Dataflow.Os =>
          throw new IllegalArgumentException("Output stationary needs output bit width as a parameter")

        case Dataflow.Ws =>
          bitWidthAdderTreeOutput + log2Ceil(arrayConfig.groupPeRow * arrayConfig.vectorPeRow)
      }

    }

    val portBitWidthInfo = PortBitWidthInfo(
      bitWidthPortA,
      bitWidthPortB,
      bitWidthMultiplierOutput,
      bitWidthAdderTreeOutput,
      enableUserBitWidth,
      bitWidthPortC
    )


    AppConfig(
      splitVerilogOutput = splitVerilogOutput,
      dataflow = dataflow,
      arrayConfig = arrayConfig,
      integerType = integerType,
      portBitWidthInfo = portBitWidthInfo
    )

  }

}
