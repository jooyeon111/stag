package stag.output

import chisel3._

trait ProcessingElementOperation[InputTypeA <: Data, InputTypeB <: Data, AdderOutputType <: Data, OutputTypeC <: Data] {

  def add(adderOutputType: AdderOutputType, outputC: OutputTypeC): OutputTypeC
  def zero: OutputTypeC

}

object ProcessingElementOperation {

  implicit val sIntProcessingElementOperation: ProcessingElementOperation[SInt, SInt, SInt, SInt] = new ProcessingElementOperation[SInt, SInt, SInt, SInt] {
    override def add(partialSum: SInt, outputC: SInt): SInt = partialSum + outputC
    override def zero: SInt = 0.S
  }

  implicit val uIntProcessingElementOperation: ProcessingElementOperation[UInt, UInt, UInt, UInt] = new ProcessingElementOperation[UInt, UInt, UInt, UInt] {
    override def add(partialSum: UInt, outputC: UInt): UInt = partialSum + outputC
    override def zero: UInt = 0.U
  }

}
