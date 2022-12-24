package fpu

object Main extends App {
    (new chisel3.stage.ChiselStage).emitVerilog(new ArrayDivider(4, Seq(2)), args)
}
