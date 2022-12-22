package fpu

object Main extends App {
    (new chisel3.stage.ChiselStage).emitVerilog(new FastAdderPipelined(32, 8), args)
}
