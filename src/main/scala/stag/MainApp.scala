package stag

import _root_.circt.stage.ChiselStage
import stag.Dataflow._
import stag.StaHierarchy._
import stag.common.SystolicTensorArrayConfig
import stag.common.PortConfig

import scala.util.{Failure, Success}

object MainApp extends App {

  val help: String = "\nFirst argument is systolic tensor array configuration" +
  "\nPut systolic tensor array configuration files in resource directory"

  if (args.isEmpty) {
    Console.err.println("No argument is provided" + help)
    sys.exit(1)
  } else if(args.length > 1){
    Console.err.println("Too many arguments are provided" + help)
    sys.exit(1)
  }

  val fileName = args(0)
  val result = ConfigParser.parseConfigFile(fileName)

  result match {
    case Success(config) =>
      val hierarchy = config.getString("Hierarchy").get match {
        case "sta" => StaHierarchy.sta
        case "sta_pod" => StaHierarchy.staPod
        case _ =>
          Console.err.println("Invalid systolic tensor array hierarchy")
          sys.exit(1)
      }
      val dataflow = config.getString("Dataflow").get match {
        case "IS" => Dataflow.Is
        case "OS" => Dataflow.Os
        case "WS" => Dataflow.Ws
        case _ =>
          Console.err.println("Invalid dataflow")
          sys.exit(1)
      }
      val r = config.getInt("R").getOrElse(-1)
      val c = config.getInt("C").getOrElse(-1)
      val a = config.getInt("A").getOrElse(-1)
      val b = config.getInt("B").getOrElse(-1)
      val p = config.getInt("P").getOrElse(-1)
      val bandwidthPortA = config.getInt("Port A").getOrElse(-1)
      val bandwidthPortB = config.getInt("Port B").getOrElse(-1)
      val bandwidthPortC = config.getInt("Port C").getOrElse(-1)

//      assert(dataflow != "Unknown", s"Cannot read dataflow in configuration file $fileName")
      assert(r != -1, s"Cannot read systolic tensor array array row in configuration file $fileName")
      assert(c != -1, s"Cannot read systolic tensor array array column in configuration file $fileName")
      assert(a != -1, s"Cannot read systolic tensor array block row in configuration file $fileName")
      assert(b != -1, s"Cannot read systolic tensor array block column in configuration file $fileName")
      assert(p != -1, s"Cannot read systolic tensor array number of multipliers in processing element in configuration file $fileName")

      assert(bandwidthPortA != -1, s"Cannot read bandwidth of port A in configuration file $fileName")
      assert(bandwidthPortB != -1, s"Cannot read bandwidth of port B in configuration file $fileName")
      assert(bandwidthPortC != -1, s"Cannot read bandwidth of port C in configuration file $fileName")

      val arrayConfig = SystolicTensorArrayConfig(r, c, a, b, p)
      val portConfig = PortConfig(bandwidthPortA, bandwidthPortB, bandwidthPortC)

      hierarchy match {
        case StaHierarchy.sta =>
          generateStaRtl(dataflow, arrayConfig, portConfig)
        case StaHierarchy.staPod =>
          generateSTaPodRtl(dataflow, arrayConfig, portConfig)
      }

    case Failure(_) =>
      Console.err.println("Cannot read config parser file")
      sys.exit(1)

  }

  private def generateStaRtl(dataflow : Dataflow, arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig) = {
    val arrayConfigString: String = s"{${arrayConfig.arrayRow}x${arrayConfig.arrayCol}}x{${arrayConfig.blockRow}x${arrayConfig.blockCol}}x${arrayConfig.numPeMultiplier}"
    dataflow match {
      case Dataflow.Is =>
        ChiselStage.emitSystemVerilogFile(
          new stag.input.SystolicTensorArray(arrayConfig, portConfig, generateRtl = true),
          firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=output/is_sta_"+ arrayConfigString +".sv")
        )
      case Dataflow.Os =>
        ChiselStage.emitSystemVerilogFile(
          new stag.output.SystolicTensorArray(arrayConfig, portConfig, generateRtl = true),
          firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=output/is_sta_"+ arrayConfigString +".sv")
        )
      case Dataflow.Ws =>
        ChiselStage.emitSystemVerilogFile(
          new stag.weight.SystolicTensorArray(arrayConfig, portConfig, generateRtl = true),
          firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=output/is_sta_"+ arrayConfigString +".sv")
        )
    }
  }

  private def generateSTaPodRtl(dataflow: Dataflow, arrayConfig: SystolicTensorArrayConfig, portConfig: PortConfig) = {
    val arrayConfigString: String = s"{${arrayConfig.arrayRow}x${arrayConfig.arrayCol}}x{${arrayConfig.blockRow}x${arrayConfig.blockCol}}x${arrayConfig.numPeMultiplier}"
    dataflow match {
      case Dataflow.Is =>
        ChiselStage.emitSystemVerilogFile(
          new stag.input.SystolicTensorArrayPod(arrayConfig, portConfig),
          firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=output/is_sta_pod_"+ arrayConfigString +".sv")
        )
      case Dataflow.Os =>
        ChiselStage.emitSystemVerilogFile(
          new stag.output.SystolicTensorArrayPod(arrayConfig, portConfig),
          firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=output/is_sta_pod_"+ arrayConfigString +".sv")
        )
      case Dataflow.Ws =>
        ChiselStage.emitSystemVerilogFile(
          new stag.weight.SystolicTensorArrayPod(arrayConfig, portConfig),
          firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=output/is_sta_pod_"+ arrayConfigString +".sv")
        )
    }
  }

}
