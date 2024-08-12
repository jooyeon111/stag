package stag

import _root_.circt.stage.ChiselStage
import stag.common.SystolicTensorArrayConfig
import stag.common.PortConfig

import scala.util.{Failure, Success}

object MainApp extends App {

  val help: String = "\nFirst argument is systolic tensor array configuration" +
  "\nPut systolic tensor array configuration files in resource directory"

  if (args.isEmpty) {
    Console.err.println("[error] No argument is provided" + help)
    sys.exit(1)
  } else if(args.length > 1){
    Console.err.println("[error] Too many arguments are provided" + help)
    sys.exit(1)
  }

  val fileName = args(0)
  val result = ConfigParser.parseConfigFile(fileName)

  result match {
    case Success(config) =>
      val dataflow = config.getString("Dataflow").getOrElse("Unknown")
      val r = config.getInt("R").getOrElse(-1)
      val c = config.getInt("C").getOrElse(-1)
      val a = config.getInt("A").getOrElse(-1)
      val b = config.getInt("B").getOrElse(-1)
      val p = config.getInt("P").getOrElse(-1)
      val bandwidthPortA = config.getInt("Port A").getOrElse(-1)
      val bandwidthPortB = config.getInt("Port B").getOrElse(-1)
      val bandwidthPortC = config.getInt("Port C").getOrElse(-1)

      assert(dataflow != "Unknown", s"[error] Cannot read dataflow in configuration file $fileName")
      assert(r != -1, s"[error] Cannot read systolic tensor array array row in configuration file $fileName")
      assert(c != -1, s"[error] Cannot read systolic tensor array array column in configuration file $fileName")
      assert(a != -1, s"[error] Cannot read systolic tensor array block row in configuration file $fileName")
      assert(b != -1, s"[error] Cannot read systolic tensor array block column in configuration file $fileName")
      assert(p != -1, s"[error] Cannot read systolic tensor array number of multipliers in processing element in configuration file $fileName")

      assert(bandwidthPortA != -1, s"[error] Cannot read bandwidth of port A in configuration file $fileName")
      assert(bandwidthPortB != -1, s"[error] Cannot read bandwidth of port B in configuration file $fileName")
      assert(bandwidthPortC != -1, s"[error] Cannot read bandwidth of port C in configuration file $fileName")

      val arrayConfig = SystolicTensorArrayConfig(r, c, a, b, p)
      val portConfig = PortConfig(bandwidthPortA, bandwidthPortB, bandwidthPortC)

      dataflow match {
        case "IS" =>
          ChiselStage.emitSystemVerilogFile(
            new stag.input.SystolicTensorArray(arrayConfig, portConfig, generateRtl = true),
            firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=output/is_sta_{${r}x$c}x{${a}x$b}x$p.sv")
          )
        case "OS" =>
          ChiselStage.emitSystemVerilogFile(
            new stag.output.SystolicTensorArray(arrayConfig, portConfig, generateRtl = true),
            firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=output/os_sta_{${r}x$c}x{${a}x$b}x$p.sv")
          )
        case "WS" =>
          ChiselStage.emitSystemVerilogFile(
            new stag.weight.SystolicTensorArray(arrayConfig, portConfig, generateRtl = true),
            firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info", s"-o=output/ws_sta_{${r}x$c}x{${a}x$b}x$p.sv")
          )
      }

    case Failure(_) =>
      Console.err.println("[error] Cannot read config parser file")
      sys.exit(1)

  }
}
