package fpu

import chisel3._

class FA extends Module {
    val io = IO(new Bundle {
        val a = Input(Bool())
        val b = Input(Bool())
        val cin = Input(Bool())

        val s = Output(Bool())
        val cout = Output(Bool())
    })

    val axorb = io.a ^ io.b

    io.s := axorb ^ io.cin
    io.cout := (io.a & io.b) || (axorb & io.cin)
}

object FA {
    def apply(a: Bool, b: Bool, cin: Bool) = {
        val fa = Module (new FA)
        fa.io.a := a
        fa.io.b := b
        fa.io.cin := cin

        (fa.io.s, fa.io.cout)
    }
}

class RippleCarryAdder(val width: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))
        val cin = Input(UInt(1.W))

        val s = Output(UInt(width.W))
        val cout = Output(UInt(1.W))
    })

    val ss = Wire(Vec(width, Bool()))
    val cs = Wire(Vec(width, Bool()))

    for (i <- 0 until width) {
        val (si, ci) = FA(io.a(i).asBool, io.b(i).asBool, if (i == 0) io.cin.asBool else cs(i-1))
        ss(i) := si
        cs(i) := ci
    }

    io.s := ss.asUInt
    io.cout := cs.last.asUInt
}

object RippleCarryAdder {
    def apply (width: Int) (a: UInt, b: UInt, cin: UInt) = {
        val mod = Module(new RippleCarryAdder(width))
        mod.io.a := a
        mod.io.b := b
        mod.io.cin := cin
        (mod.io.s, mod.io.cout)
    }
}

class FastAdderPipelined(val width: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))
        val cin = Input(UInt(1.W))

        val Sum = Output(UInt(width.W))
        val Cout = Output(UInt(1.W))
    })

    val gpts_0 = GPTGen(width)(io.a, io.b)

    val groups = VecInit(gpts_0.grouped(4).toVector.map{
        case group => ReduceNibble(VecInit(group))
    })

    def reduce(gpts: Vec[Vec[GPT]], cin: Bool) : (Vec[Vec[GPT]], Bool) = {
        def nextLayer(prev: Vec[Vec[GPT]], cin: Bool, offset: Int) : (Vec[Vec[GPT]], Bool) = {
            if (offset > prev.size) {
                (RegNext(prev), RegNext(cin))
            } else {
                val layer = VecInit(Vector.tabulate(prev.size) { i =>
                    if (i < offset) {
                        prev(i)
                    } else {
                        val (grpi, grpj) = (prev(i), prev(i - offset))
                        val gptj = grpj.last
                        VecInit(grpi.map{ case gpti => GPTInit (gpti.g | (gpti.p & gptj.g), gpti.p & gptj.p, gpti.t) })
                    }
                })

                val reg = RegNext(layer)
                val cinreg = RegNext(cin)
                nextLayer(reg, cinreg, offset<<1)
            }
        }

        nextLayer(gpts, cin, 1)
    }

    val (grps_last, cin) = reduce(groups, io.cin.asBool)
    val gpts_last = grps_last.flatten

    val gs = gpts_last.map(_.g)
    val ps = gpts_last.map(_.p)
    val ts = gpts_last.map(_.t)

    val cs = cin +: gs.zip(ps).map{ case (gi, pi) => gi | (pi & cin) }

    val ss = for (i <- 0 until width) yield {
        cs(i) ^ ts(i)
    }

    io.Sum := RegNext(VecInit(ss).asUInt)
    io.Cout := RegNext(cs.last)
}

object FastAdderPipelined {
    def apply (width: Int) (a: UInt, b: UInt, cin: UInt) : (UInt, UInt) = {
        val mod = Module (new FastAdderPipelined(width))
        mod.io.a := a
        mod.io.b := b
        mod.io.cin := cin

        (mod.io.Sum, mod.io.Cout)
    }
}