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

  case class PortBitWidthInfo(
    bitWidthPortA: Int,
    bitWidthPortB: Int,
    bitWidthMultiplierOutput: Int,
    bitWidthAdderTreeOutput: Int,
    enableUserBitWidth: Boolean,
    bitWidthPortC: Int
  )

  case class AppConfig (
    hierarchy: StaHierarchy.Value,
    dataflow: Dataflow.Value,
    arrayConfig: SystolicTensorArrayConfig,
    integerType: IntegerType.Value,
    portBitWidthInfo: PortBitWidthInfo
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

    val bitWidthPortA = config.getInt("Port A").get
    val bitWidthPortB = config.getInt("Port B").get
    val bitWidthMultiplierOutput = bitWidthPortA + bitWidthPortB
    val bitWidthAdderTreeOutput = bitWidthMultiplierOutput + log2Ceil(arrayConfig.numPeMultiplier)
    val bitWidthPortC = config.getInt("Port C").getOrElse(-1)

    val enableUserBitWidth = if(bitWidthPortC.isValidInt){
      if (bitWidthPortC == -1)
        false
      else
        true
    } else
      false

    if(enableUserBitWidth)
      println("Use user define bit width")
    else
      println("Use default bit width")

    val staBitWidthPortC = if(enableUserBitWidth) {
      config.getInt("Port C").get
    } else {
      dataflow match {
        case Dataflow.Is =>
          bitWidthPortA + bitWidthPortB + log2Ceil(arrayConfig.numPeMultiplier) + arrayConfig.groupPeCol * arrayConfig.vectorPeCol

        case Dataflow.Os =>
          if(bitWidthPortA > bitWidthPortB)
            bitWidthPortA
          else
            bitWidthPortB

        case Dataflow.Ws =>
          bitWidthPortA + bitWidthPortB + log2Ceil(arrayConfig.numPeMultiplier) + arrayConfig.groupPeRow * arrayConfig.vectorPeRow

      }
    }

    assert(staBitWidthPortC >= bitWidthPortA,
      s"Output port bit width is too small output port C: $staBitWidthPortC input port A: $bitWidthPortA")
    assert(staBitWidthPortC >= bitWidthPortB,
      s"Output port bit width is too small output port C: $staBitWidthPortC input port B: $bitWidthPortB")

    val portBitWidthInfo = PortBitWidthInfo(
      bitWidthPortA,
      bitWidthPortB,
      bitWidthMultiplierOutput,
      bitWidthAdderTreeOutput,
      enableUserBitWidth,
      staBitWidthPortC
    )


    AppConfig(hierarchy = hierarchy, dataflow = dataflow, arrayConfig = arrayConfig, integerType = integerType, portBitWidthInfo = portBitWidthInfo)

  }


  def generateRtl(appConfig: AppConfig): Unit = {
    val AppConfig(hierarchy, dataflow, arrayConfig, integerType, portBitWidthInfo) = appConfig

    integerType match {
      case IntegerType.Signed =>
        generateRtlForType[SInt](arrayConfig, portBitWidthInfo, hierarchy, dataflow, (w:Int) => SInt(w.W))
      case IntegerType.UnSigned =>
        generateRtlForType[UInt](arrayConfig, portBitWidthInfo, hierarchy, dataflow, (w:Int) => UInt(w.W))
    }
  }

  def generateRtlForType[T <: Data](
     arrayConfig: SystolicTensorArrayConfig,
     portBitWidthInfo: PortBitWidthInfo,
     hierarchy: StaHierarchy.Value,
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

    val prefix = if (hierarchy == StaHierarchy.DimensionAlignedSta) "dimension_aligned_" else ""
    val dataflowString = dataflow.toString.toLowerCase
    val generatedFileName = s"${prefix}${dataflowString}_sta_${arrayConfig.arrayConfigString}"

    lazy val rtlGenerator =  (hierarchy, dataflow) match {
      case (StaHierarchy.Sta, Dataflow.Is) =>
        new stag.input.SystolicTensorArray(arrayConfig, portConfig, generateRtl = true)
      case (StaHierarchy.Sta, Dataflow.Os) =>
        new stag.output.SystolicTensorArray(arrayConfig, portConfig, generateRtl = true)
      case (StaHierarchy.Sta, Dataflow.Ws) =>
        new stag.weight.SystolicTensorArray(arrayConfig, portConfig, generateRtl = true)
      case (StaHierarchy.DimensionAlignedSta, Dataflow.Is) =>
        new stag.input.DimensionAlignedSystolicTensorArray(arrayConfig, generatedFileName, portConfig)
      case (StaHierarchy.DimensionAlignedSta, Dataflow.Os) =>
        new stag.output.DimensionAlignedSystolicTensorArray(arrayConfig, generatedFileName, portConfig)
      case (StaHierarchy.DimensionAlignedSta, Dataflow.Ws) =>
        new stag.weight.DimensionAlignedSystolicTensorArray(arrayConfig, generatedFileName, portConfig)
    }
    
    ChiselStage.emitSystemVerilogFile(
      rtlGenerator,
//      firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=output/$generatedFileName/", "-split-verilog")
      firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=./output/$generatedFileName")
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
