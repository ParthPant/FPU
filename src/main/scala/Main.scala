package fpu

object Main extends App {
    // (new chisel3.stage.ChiselStage).emitVerilog(new FastAdderPipelined(32, 1), Array("--target-dir", "verilog/", "-o", "PFA_1"))
    (new chisel3.stage.ChiselStage).emitVerilog(new FastAdderPipelined(32, 2), Array("--target-dir", "verilog/", "-o", "PFA_2"))
    // (new chisel3.stage.ChiselStage).emitVerilog(new FastAdderPipelined(32, 4), Array("--target-dir", "verilog/", "-o", "PFA_4"))
    // (new chisel3.stage.ChiselStage).emitVerilog(new FastAdderPipelined(32, 8), Array("--target-dir", "verilog/", "-o", "PFA_8"))
    // (new chisel3.stage.ChiselStage).emitVerilog(new FastAdderPipelined(32, 16), Array("--target-dir", "verilog/", "-o", "PFA_16"))
    // (new chisel3.stage.ChiselStage).emitVerilog(new FastAdderPipelined(32, 32), Array("--target-dir", "verilog/", "-o", "PFA_32"))
}
