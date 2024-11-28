package stag.common

trait VerilogNaming {
  protected def camelToSnake(name: String): String = {
    name.replaceAll("([A-Z])", "_$1")
      .toLowerCase
      .replaceAll("^_", "")
  }
}
