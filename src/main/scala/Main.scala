package fpu

import fpu.util._

object Main extends App {
  val stage = new chisel3.stage.ChiselStage
  stage.emitVerilog(
    new FRoot,
    Array("--target-dir", "verilog/", "-o", "FRoot")
  )
  stage.emitVerilog(
    new FMul,
    Array("--target-dir", "verilog/", "-o", "FMul")
  )
  stage.emitVerilog(
    new FDiv,
    Array("--target-dir", "verilog/", "-o", "FDiv")
  )
  stage.emitVerilog(
    new FAddSub,
    Array("--target-dir", "verilog/", "-o", "FAddSub")
  )
  // stage.emitVerilog(
  //   new subscalar.Adder(8),
  //   Array("--target-dir", "verilog/", "-o", "Adder")
  //   )
}
