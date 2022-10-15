package fpu

import chisel3._
import chisel3.util._

class GPT extends Bundle {
    val g = UInt(1.W)
    val p = UInt(1.W)
    val t = UInt(1.W)
}

object GPTInit {
    def apply(g: UInt, p: UInt, t: UInt) = {
        val bun = Wire(new GPT)
        bun.g := g
        bun.p := p
        bun.t := t
        bun
    }
}

class GPTGen(val width: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))

        val gpts = Output(Vec(width, new GPT))
    })

    val as = io.a.asBools
    val bs = io.b.asBools
    io.gpts := VecInit(as.zip(bs)
                .map{ case (ai, bi) => (ai & bi, ai || bi, ai ^ bi) }
                .map{ 
                    case x => {
                        GPTInit(x._1, x._2, x._3)
                    }
                })
}

object GPTGen {
    def apply(width: Int)(a: UInt, b: UInt) = {
        val mod = Module (new GPTGen(width))
        mod.io.a := a
        mod.io.b := b
        mod.io.gpts
    }
}

object ParallelPrefixTree {
    def apply[T](summands: Seq[T])(associativeOp: (T, T) => T) : Vector[T] = {
        def helper(offset: Int, x: Vector[T]) : Vector[T] = {
            if (offset > x.size) {
                x
            } else {
                val layer = Vector.tabulate(x.size) { i =>
                    if (i < offset) {
                        x(i)
                    } else {
                        associativeOp(x(i), x(i-offset))
                    }
                }

                helper(offset << 1, layer)
            }
        }

        helper(1, summands.toVector)
    }
}

class Reduce(val width: Int, val valency: Int) extends Module {
    val io = IO(new Bundle {
        val gptin  = Input (Vec(width, new GPT))
        val gptout = Output(Vec(width, new GPT))
    })

    if (valency == 1) {
        val gpts = ParallelPrefixTree(io.gptin) {
            case (gpti, gptj) => GPTInit(gpti.g | (gpti.p & gptj.g), gpti.p & gptj.p, gpti.t)
        }
        io.gptout := VecInit(gpts)
    } else {
        val groups = io.gptin.grouped(valency).toVector.map{ case group => Reduce(valency, 1)(VecInit(group)) }
        val gpts = ParallelPrefixTree(groups) {
            case (grpi, grpj) => {
                val gptj = grpj.last
                VecInit(grpi.map{ case gpti => GPTInit (gpti.g | (gpti.p & gptj.g), gpti.p & gptj.p, gpti.t) })
            }
        }.flatten
        io.gptout := VecInit(gpts)
    }
}

object Reduce {
    def apply(width: Int, valency: Int)(gpts: Vec[GPT]) = {
        val mod = Module (new Reduce(width, valency))
        mod.io.gptin := gpts
        mod.io.gptout
    }
}

class FastAdder(val width: Int, val valency: Int) extends Module {
    val io = IO(new Bundle {
        val a = Input(UInt(width.W))
        val b = Input(UInt(width.W))
        val cin = Input(UInt(1.W))

        val Sum = Output(UInt(width.W))
        val Cout = Output(UInt(1.W))
    })

    val gpts_0 = GPTGen(width)(io.a, io.b)
    val gpts_last = Reduce(width, valency)(gpts_0)

    val gs = gpts_last.map(_.g)
    val ps = gpts_last.map(_.p)
    val ts = gpts_last.map(_.t)

    val cs = io.cin +: gs.zip(ps).map{ case (gi, pi) => gi | (pi & io.cin.asBool) }

    val ss = for (i <- 0 until width) yield {
        cs(i) ^ ts(i)
    }

    io.Sum := VecInit(ss).asUInt
    io.Cout := cs.last
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
        case group => VecInit(ParallelPrefixTree(group) {
            case (gpti, gptj) => GPTInit(gpti.g | (gpti.p & gptj.g), gpti.p & gptj.p, gpti.t)
        })
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