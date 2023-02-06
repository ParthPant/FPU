package fpu

import chisel3._
import util._

class FRoot extends Module {
  val io = IO(new Bundle {
    val a = Input(new FloatingPoint)
    val out = Output(new FloatingPoint)
  })
}
