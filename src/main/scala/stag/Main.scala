package stag

import chisel3._
import _root_.circt.stage.ChiselStage
import stag.common.{Arithmetic, PortConfig, SystolicTensorArrayConfig}
import stag.common.Arithmetic._
import scala.util.{Failure, Success}

object Main extends App with ConfigurationParser {

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
