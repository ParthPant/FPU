package fpu

import chisel3._
import chisel3.util._

// Implementation follows this research paper: https://www.researchgate.net/publication/284919835_Modular_Design_Of_Fast_Leading_Zeros_Counting_Circuit

class NLC extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(4.W))
        val Z = Output(UInt(2.W))
        val a = Output(UInt(1.W))
    })

    val ins = io.in.asBools
    val Zs = VecInit(~((~ins(2) & ins(1)) || ins(3)), ~(ins(2) || ins(3)))

    io.a := ~(ins.reduce(_ || _))
    io.Z := Zs.asUInt
}

class CLZ16 extends Module {
    val io = IO(new Bundle {
      val in = Input(UInt(16.W))
      val Z = Output(UInt(4.W))
      val a = Output(UInt(1.W))
    })

    val ins = io.in.asBools

    val zs = Wire(Vec(4, UInt(2.W)))
    val as = Wire(Vec(4, Bool()))
    
    for (i <- 0 until 4) {
        val mod = Module (new NLC)
        val j = 3 - i
        mod.io.in := VecInit(ins.slice(4*j, 4*j+4)).asUInt
        zs(i) := mod.io.Z
        as(i) := ~(mod.io.a.asBool)
    }

    val BNEout = PriorityEncoder(as.asUInt)
    val MUXout = PriorityMux(as.zip(zs))

    val out = Cat(BNEout, MUXout)

    io.a := ~as.reduce(_ || _).asUInt
    io.Z := VecInit(out.asBools).asUInt
}

class CLZ24 extends Module {
    val io = IO(new Bundle {
      val in = Input(UInt(24.W))
      val Z = Output(UInt(5.W))
      val a = Output(UInt(1.W))
    })

    val ins = io.in.asBools

    val zs = Wire(Vec(6, UInt(2.W)))
    val as = Wire(Vec(6, Bool()))
    
    for (i <- 0 until 6) {
        val mod = Module (new NLC)
        val j = 5 - i
        mod.io.in := VecInit(ins.slice(4*j, 4*j+4)).asUInt
        zs(i) := mod.io.Z
        as(i) := ~(mod.io.a.asBool)
    }

    val BNEout = PriorityEncoder(as.asUInt)
    val MUXout = PriorityMux(as.zip(zs))

    val out = Cat(BNEout, MUXout)

    io.a := ~as.reduce(_ || _).asUInt
    io.Z := VecInit(out.asBools).asUInt
}

class CLZ32 extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(32.W))
        val Z = Output(UInt(5.W))
        val a = Output(UInt(1.W))
    })

    val ins = io.in.asBools

    val zs = Wire(Vec(8, UInt(2.W)))
    val as = Wire(Vec(8, Bool()))

    for (i <- 0 until 8) {
        val mod = Module (new NLC)
        val j = 7 - i
        mod.io.in := VecInit(ins.slice(4*j, 4*j+4)).asUInt     
        zs(i) := mod.io.Z
        as(i) := ~(mod.io.a.asBool)
    }

    val BNEout = PriorityEncoder(as.asUInt)
    val MUXout = PriorityMux(as.zip(zs))

    val out = Cat(BNEout, MUXout)

    io.a := ~as.reduce(_ || _).asUInt
    io.Z := VecInit(out.asBools).asUInt
}

class CLZ48 extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(48.W))
        val Z = Output(UInt(6.W))
        val a = Output(UInt(1.W))
    })

    val ins = io.in.asBools

    val zs = Wire(Vec(12, UInt(2.W)))
    val as = Wire(Vec(12, Bool()))

    for (i <- 0 until 12) {
        val mod = Module (new NLC)
        val j = 11 - i
        mod.io.in := VecInit(ins.slice(4*j, 4*j+4)).asUInt     
        zs(i) := mod.io.Z
        as(i) := ~(mod.io.a.asBool)
    }

    val BNEout = PriorityEncoder(as.asUInt)
    val MUXout = PriorityMux(as.zip(zs))

    val out = Cat(BNEout, MUXout)

    io.a := ~as.reduce(_ || _).asUInt
    io.Z := VecInit(out.asBools).asUInt
}

class CLZ64 extends Module {
    val io = IO(new Bundle {
        val in = Input(UInt(64.W))
        val Z = Output(UInt(6.W))
        val a = Output(UInt(1.W))
    })

    val ins = io.in.asBools

    val CLZ32H = Module (new CLZ32)
    val CLZ32L = Module (new CLZ32)

    CLZ32L.io.in := VecInit(ins.slice(0, 32)).asUInt
    CLZ32H.io.in := VecInit(ins.slice(32, 64)).asUInt

    io.a := CLZ32H.io.a & CLZ32L.io.a
    io.Z := Cat(CLZ32H.io.a, Mux(CLZ32H.io.a.asBool, CLZ32L.io.Z, CLZ32H.io.Z))
}
