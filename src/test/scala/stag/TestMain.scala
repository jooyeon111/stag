package stag

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import coursier.core.shaded.fastparse.Parsed.Success
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import stag.common.{Arithmetic, PortConfig, SystolicTensorArrayConfig}
import stag.output.DimensionAlignedSystolicTensorArray

import scala.util.Random
import scala.util.Failure

object TestMain extends AnyFreeSpec with Matchers with ConfigurationParser with App {

  def testRtl(appConfig: AppConfig): Unit = {

    appConfig.dataflow match {
      case Dataflow.Is =>
        appConfig.integerType match {
          case IntegerType.Signed =>
            testIsStaRTl[SInt](appConfig.arrayConfig, appConfig.portBitWidthInfo, (w: Int) => SInt(w.W))
          case IntegerType.UnSigned =>
            testIsStaRTl[UInt](appConfig.arrayConfig, appConfig.portBitWidthInfo, (w: Int) => UInt(w.W))
        }

      case Dataflow.Os =>
        appConfig.integerType match {
          case IntegerType.Signed =>
            testOsStaRTl[SInt](appConfig.arrayConfig, appConfig.portBitWidthInfo, (w: Int) => SInt(w.W))
          case IntegerType.UnSigned =>
            testOsStaRTl[UInt](appConfig.arrayConfig, appConfig.portBitWidthInfo, (w: Int) => UInt(w.W))
        }

      case Dataflow.Ws =>
        appConfig.integerType match {
          case IntegerType.Signed =>
            testWsStaRTl[SInt](appConfig.arrayConfig, appConfig.portBitWidthInfo, (w: Int) => SInt(w.W))
          case IntegerType.UnSigned =>
            testWsStaRTl[UInt](appConfig.arrayConfig, appConfig.portBitWidthInfo, (w: Int) => UInt(w.W))
          case _ =>
            throw new Exception("Wrong dataflow")
        }
    }

  }

  def testIsStaRTl[T <: Data](
     arrayConfig: SystolicTensorArrayConfig,
     portBitWidthInfo: PortBitWidthInfo,
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
    val dataflowString = Dataflow.Is.toString.toLowerCase
    val generatedFileName = s"${dataflowString}_sta_${arrayConfig.arrayConfigString}"

    val arrayRowDimension = arrayConfig.groupPeRow * arrayConfig.vectorPeRow * arrayConfig.numPeMultiplier
    val arrayColDimension = arrayConfig.groupPeCol * arrayConfig.vectorPeCol * arrayConfig.numPeMultiplier

    val matrixA = Vector.fill(arrayRowDimension, arrayColDimension)(Random.nextInt(100))
    val matrixB = Vector.fill(arrayColDimension, arrayRowDimension)(Random.nextInt(100))


    "Input stationary systolic tensor array calculate AxB=C GEMM" in {
      simulate(new input.DimensionAlignedSystolicTensorArray(arrayConfig, generatedFileName, portConfig)){ dut =>


        for ( i <- 0 until arrayConfig.groupPeCol * arrayConfig.vectorPeCol)
          dut.io.propagateA(i).poke(true.B)

        dut.clock.step(1)

        for ( j <- 0 until arrayColDimension){
          for ( i <- 0 until arrayRowDimension){
            dut.io.inputA(i).poke(matrixA(i)(j))
            dut.io.inputB(i).poke(matrixB(i)(j))
          }

          if( j == arrayColDimension){
            for ( i <- 0 until arrayConfig.groupPeCol * arrayConfig.vectorPeCol)
              dut.io.propagateA(i).poke(false.B)
          }

          dut.clock.step(1)

        }










      }
    }


  }

  def testOsStaRTl[T <: Data](
    arrayConfig: SystolicTensorArrayConfig,
    portBitWidthInfo: PortBitWidthInfo,
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
    val dataflowString = Dataflow.Is.toString.toLowerCase
    val generatedFileName = s"${dataflowString}_sta_${arrayConfig.arrayConfigString}"

    "Output stationary systolic tensor array calculate AxB=C GEMM" in {
      simulate(new output.DimensionAlignedSystolicTensorArray(arrayConfig, generatedFileName, portConfig)){ dut =>

      }
    }

  }

  def testWsStaRTl[T <: Data](
    arrayConfig: SystolicTensorArrayConfig,
    portBitWidthInfo: PortBitWidthInfo,
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
    val dataflowString = Dataflow.Is.toString.toLowerCase
    val generatedFileName = s"${dataflowString}_sta_${arrayConfig.arrayConfigString}"

    "Weight stationary systolic tensor array calculate AxB=C GEMM" in {
      simulate(new weight.DimensionAlignedSystolicTensorArray(arrayConfig, generatedFileName, portConfig)){ dut =>

      }
    }
  }

  parseArgs(args) match {
    case Right(fileName) =>
      ConfigParser.parseConfigFile(fileName) match {
        case scala.util.Success(config) =>
          parseConfig(config) match {
            case scala.util.Success(appConfig) => testRtl(appConfig)
            case Failure(e) => Console.err.println(s"Error parsing config: ${e.getMessage}")
          }
        case Failure(e) => Console.err.println(s"Cannot read config parser file ${e.getMessage}")
      }

    case Left(error) => Console.err.println(error + "\nPut Systolic tensor array configuration files in resource directory")
  }

}

