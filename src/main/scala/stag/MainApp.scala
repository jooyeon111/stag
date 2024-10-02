package stag

import _root_.circt.stage.ChiselStage
import stag.common.{Arithmetic, PortConfig, SystolicTensorArrayConfig}
import chisel3._
import chisel3.util.log2Ceil
import stag.common.Arithmetic._
import scala.util.{Failure, Success, Try}

object MainApp extends App {

  object Dataflow extends Enumeration {
    type Dataflow = Value
    val Is, Os, Ws = Value
  }

  object IntegerType extends Enumeration {
    type IntegerType = Value
    val Signed, UnSigned = Value
  }

  object StaHierarchy extends Enumeration {
    type StaHierarchy = Value
    val Sta, DimensionAlignedSta, StaEngine = Value
  }

  case class InputPortBitWidth (
    bitWidthPortA: Int,
    bitWidthPortB: Int
  )

  case class AppConfig (
    hierarchy: StaHierarchy.Value,
    dataflow: Dataflow.Value,
    arrayConfig: SystolicTensorArrayConfig,
    integerType: IntegerType.Value,
    portBitWidth: InputPortBitWidth,
  )

  def parseArgs(args: Array[String]): Either[String, String] = {
    args match {
      case Array(fileName) => Right(fileName)
      case Array() => Left("No argument is provided")
      case _ => Left("Too many arguments are provided")
    }
  }

  def parseConfig(config: ConfigParser.Config): Try[AppConfig] = Try {

    val hierarchy = config.getString("Hierarchy").get match {
      case "sta" => StaHierarchy.Sta
      case "dimension_aligned_sta" => StaHierarchy.DimensionAlignedSta
      case _ => throw new IllegalArgumentException("Invalid systolic tensor array hierarchy")
    }

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

    val inputPortBitWidth = InputPortBitWidth(
      config.getInt("Port A").get,
      config.getInt("Port B").get,
    )

    AppConfig(hierarchy = hierarchy, dataflow = dataflow, arrayConfig = arrayConfig, integerType = integerType, portBitWidth = inputPortBitWidth)
  }


  def generateRtl(appConfig: AppConfig): Unit = {
    val AppConfig(hierarchy, dataflow, arrayConfig, integerType, portBitWidth) = appConfig
    val arrayConfigString = s"{${arrayConfig.groupPeRow}x${arrayConfig.groupPeCol}}x{${arrayConfig.vectorPeRow}x${arrayConfig.vectorPeCol}}x${arrayConfig.numPeMultiplier}"

    integerType match {
      case IntegerType.Signed =>
        generateRtlForType[SInt](arrayConfig, portBitWidth, hierarchy, dataflow, arrayConfigString, (w:Int) => SInt(w.W))
      case IntegerType.UnSigned =>
        generateRtlForType[UInt](arrayConfig, portBitWidth, hierarchy, dataflow, arrayConfigString, (w:Int) => UInt(w.W))
    }
  }

  def generateRtlForType[T <: Data](
     arrayConfig: SystolicTensorArrayConfig,
     portBitWidth: InputPortBitWidth,
     hierarchy: StaHierarchy.Value,
     dataflow: Dataflow.Value,
     arrayConfigString: String,
     typeConstructor: Int => T
   )(implicit ev: Arithmetic[T]): Unit = {

    val inputTypeA = typeConstructor(portBitWidth.bitWidthPortA)
    val inputTypeB = typeConstructor(portBitWidth.bitWidthPortB)
    val multiplierOutputType = typeConstructor(portBitWidth.bitWidthPortA + portBitWidth.bitWidthPortB)
    val adderTreeOutputType = typeConstructor(portBitWidth.bitWidthPortA + portBitWidth.bitWidthPortB + log2Ceil(arrayConfig.numPeMultiplier))
    val outputTypeC = typeConstructor(32)

    val portConfig = PortConfig(
      inputTypeA,
      inputTypeB,
      multiplierOutputType,
      adderTreeOutputType,
      outputTypeC
    )

    lazy val rtlGenerator =  (hierarchy, dataflow) match {
      case (StaHierarchy.Sta, Dataflow.Is) =>
        new stag.input.SystolicTensorArray(arrayConfig, portConfig, generateRtl = true)
      case (StaHierarchy.Sta, Dataflow.Os) =>
        new stag.output.SystolicTensorArray(arrayConfig, portConfig, generateRtl = true)
      case (StaHierarchy.Sta, Dataflow.Ws) =>
        new stag.weight.SystolicTensorArray(arrayConfig, portConfig, generateRtl = true)
      case (StaHierarchy.DimensionAlignedSta, Dataflow.Is) =>
        new stag.input.DimensionAlignedSystolicTensorArray(arrayConfig, portConfig)
      case (StaHierarchy.DimensionAlignedSta, Dataflow.Os) =>
        new stag.output.DimensionAlignedSystolicTensorArray(arrayConfig, portConfig)
      case (StaHierarchy.DimensionAlignedSta, Dataflow.Ws) =>
        new stag.weight.DimensionAlignedSystolicTensorArray(arrayConfig, portConfig)
    }

    val prefix = if (hierarchy == StaHierarchy.DimensionAlignedSta) "dimension_aligned_" else ""
    val dataflowString = dataflow.toString.toLowerCase
    ChiselStage.emitSystemVerilogFile(
      rtlGenerator,
      firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=output/${prefix}${dataflowString}_sta_$arrayConfigString.sv")
    )

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
