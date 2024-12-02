package stag

import _root_.circt.stage.ChiselStage
import stag.common.{Arithmetic, PortConfig, SystolicTensorArrayConfig}
import chisel3._
import chisel3.util.log2Ceil
import stag.common.Arithmetic._
import scala.util.{Failure, Success, Try}

object Main extends App {

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

  def generateRtl(appConfig: AppConfig): Unit = {
    val AppConfig(splitVerilogOutput, dataflow, arrayConfig, integerType, portBitWidthInfo) = appConfig

    integerType match {
      case IntegerType.Signed =>
        generateRtlForType[SInt](splitVerilogOutput, arrayConfig, portBitWidthInfo, dataflow, (w: Int) => SInt(w.W))

      case IntegerType.UnSigned =>
        generateRtlForType[UInt](splitVerilogOutput, arrayConfig, portBitWidthInfo, dataflow, (w: Int) => UInt(w.W))
    }
  }

  def generateRtlForType[T <: Data](
     splitVerilogOutput: Boolean,
     arrayConfig: SystolicTensorArrayConfig,
     portBitWidthInfo: PortBitWidthInfo,
     dataflow: Dataflow.Value,
     typeConstructor: Int => T
   )(implicit ev: Arithmetic[T]): Unit = {

    val inputTypeA = typeConstructor(portBitWidthInfo.bitWidthPortA)
    val inputTypeB = typeConstructor(portBitWidthInfo.bitWidthPortB)
    val multiplierOutputType = typeConstructor(portBitWidthInfo.bitWidthMultiplierOutput)
    val adderTreeOutputType = typeConstructor(portBitWidthInfo.bitWidthAdderTreeOutput)
    val outputTypeC = typeConstructor(portBitWidthInfo.bitWidthPortC)

    val portConfig = new PortConfig(
      inputTypeA,
      inputTypeB,
      multiplierOutputType,
      adderTreeOutputType,
      portBitWidthInfo.enableUserBitWidth,
      outputTypeC
    )

    val dataflowString = dataflow.toString.toLowerCase
    val generatedFileName = s"${dataflowString}_sta_${arrayConfig.arrayConfigString}"

    lazy val rtlGenerator =  dataflow match {
      case Dataflow.Is =>
        new stag.input.DimensionAlignedSystolicTensorArray(arrayConfig, generatedFileName, portConfig)
      case Dataflow.Os =>
        new stag.output.DimensionAlignedSystolicTensorArray(arrayConfig, generatedFileName, portConfig)
      case Dataflow.Ws =>
        new stag.weight.DimensionAlignedSystolicTensorArray(arrayConfig, generatedFileName, portConfig)
    }

    if(splitVerilogOutput){
      ChiselStage.emitSystemVerilogFile(
        rtlGenerator,
        firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=output/$generatedFileName/", "-split-verilog")
      )
    } else {
      ChiselStage.emitSystemVerilogFile(
        rtlGenerator,
        firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=./output/$generatedFileName.sv")
      )
    }

  }

  parseArgs(args) match {
    case Right(fileName) =>
      ConfigParser.parseConfigFile(fileName) match {
        case Success(config) =>
          parseConfig(config) match {
            case Success(appConfig) => generateRtl(appConfig)
            case Failure(e) => Console.err.println(s"Error parsing config: ${e.getMessage}")
          }
        case Failure(e) => Console.err.println(s"Cannot read config parser file ${e.getMessage}")
      }

    case Left(error) => Console.err.println(error + "\nPut Systolic tensor array configuration files in resource directory")
  }

}
