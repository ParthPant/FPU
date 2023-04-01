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
    val fa = Module(new FA)
    fa.io.a := a
    fa.io.b := b
    fa.io.cin := cin

    (fa.io.s, fa.io.cout)
  }
}

class FS extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val cin = Input(Bool())

    val s = Output(Bool())
    val cout = Output(Bool())
  })

  val axorb = io.a ^ io.b

  io.s := axorb ^ io.cin
  io.cout := (io.cin & ~axorb) | (~io.a & io.b)
}

object FS {
  def apply(a: Bool, b: Bool, cin: Bool) = {
    val fa = Module(new FS)
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
    val (si, ci) = FA(
      io.a(i).asBool,
      io.b(i).asBool,
      if (i == 0) io.cin.asBool else cs(i - 1)
    )
    ss(i) := si
    cs(i) := ci
  }

  io.s := ss.asUInt
  io.cout := cs.last.asUInt
}

object RippleCarryAdder {
  def apply(width: Int)(a: UInt, b: UInt, cin: UInt) = {
    val mod = Module(new RippleCarryAdder(width))
    mod.io.a := a
    mod.io.b := b
    mod.io.cin := cin
    (mod.io.s, mod.io.cout)
  }
}

class FastAdderPipelined(val width: Int, val valency: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val cin = Input(UInt(1.W))

    val Sum = Output(UInt(width.W))
    val Cout = Output(UInt(1.W))
  })

  def gptGen(pair: (Bool, Bool)): GPT = {
    val (a, b) = pair
    GPTInit(a & b, a || b, a ^ b)
  }

  val (as, bs) = (io.a.asBools, io.b.asBools)
  val gpts_0 = as.zip(bs).map(gptGen(_))
  val gpts = gpts_0
    .grouped(valency)
    .toVector
    .map { case group =>
      group.tail.scanLeft(group.head)((a, b) => b dot a)
    }
    .flatten

  def reduce(gpts: Seq[GPT], cin: Bool): (UInt, UInt) = {
    def nextLayer(prev: Seq[GPT], cin: Bool, groupoffset: Int): (UInt, UInt) = {
      if (valency * groupoffset > prev.size) {
        val (gs, ps, ts) = (prev.map(_.g), prev.map(_.p), prev.map(_.t))

        val cs = cin +: gs.zip(ps).map { case (gi, pi) => gi | (pi & cin) }
        val ss = for (i <- 0 until width) yield {
          cs(i) ^ ts(i)
        }
        (RegNext(VecInit(ss).asUInt), RegNext(cs.last.asUInt))
      } else {
        val layer = VecInit(Vector.tabulate(prev.size) { i =>
          if (i < valency * groupoffset) {
            prev(i)
          } else {
            val (gpti, gptj) = (
              prev(i),
              prev(i - i % valency - groupoffset * valency + valency - 1)
            )
            gpti dot gptj
          }
        })

        val reg = RegNext(layer)
        val cinreg = RegNext(cin)
        nextLayer(reg, cinreg, groupoffset << 1)
      }
    }

    nextLayer(gpts, cin, 1)
  }

  val (sum, cout) = reduce(gpts, io.cin.asBool)
  io.Sum := sum
  io.Cout := cout
}

object FastAdderPipelined {
  def apply(
      width: Int,
      valency: Int
  )(a: UInt, b: UInt, cin: UInt): (UInt, UInt) = {
    val mod = Module(new FastAdderPipelined(width, valency))
    mod.io.a := a
    mod.io.b := b
    mod.io.cin := cin

    (mod.io.Sum, mod.io.Cout)
  }
}
