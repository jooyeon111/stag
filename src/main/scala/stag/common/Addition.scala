package stag.common

import chisel3._

trait Addition[A <: Data, B <: Data, C <: Data] {
  def add(a: A, b: B): C
}
