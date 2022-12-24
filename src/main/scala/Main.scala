package fpu

object Main extends App {
    (new chisel3.stage.ChiselStage).emitVerilog(new FMul, args)
}
