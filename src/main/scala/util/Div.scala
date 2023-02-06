package fpu

import chisel3._
import chisel3.util._

class CAS extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val ctrl = Input(Bool())
    val cin = Input(Bool())

    val S = Output(Bool())
    val Cout = Output(Bool())
  })

  val (s, cout) = FA(io.a ^ io.ctrl, io.b, io.cin)
  io.S := s
  io.Cout := cout
}

object CAS {
  def apply(a: Bool, b: Bool, ctrl: Bool, cin: Bool): (Bool, Bool) = {
    val mod = Module(new CAS)
    mod.io.a := a
    mod.io.b := b
    mod.io.ctrl := ctrl
    mod.io.cin := cin
    (mod.io.S, mod.io.Cout)
  }
}

class NonRestoringArrayDivider(val width: Int, val stages: Seq[Int])
    extends Module {
  val io = IO(new Bundle {
    val z = Input(UInt((2 * width).W)) // dividend
    val d = Input(UInt(width.W)) // divisor

    val Q = Output(UInt(width.W))
    val S = Output(UInt(width.W))
  })

  def makeTree(z: UInt, d: UInt): (UInt, UInt) = {
    /* nextLayer
     *
     * @param ctrl -> add/sub control and cin from prev layer
     * @param z    -> partial dividend (only the lower bits that are needed for that layer and layers after that)
     * @param s    -> partial remainder after prev add/sub
     * @param d    -> divisor
     * @param q    -> quotient (each layer produces a new bit)
     */
    def nextLayer(
        ctrl: Bool,
        z: Seq[Bool],
        s: UInt,
        d: UInt,
        q: Seq[Bool],
        depth: Int
    ): (UInt, UInt) = {
      if (depth < width) {
        val cs = Wire(Vec(width + 1, Bool()))
        val ns = Wire(Vec(width + 1, Bool()))

        for (i <- 0 to width) {
          val (ni, ci) = CAS(d(i), s(i), ctrl, if (i == 0) ctrl else cs(i - 1))
          ns(i) := ni
          cs(i) := ci
        }

        val snext =
          if (z.isEmpty) ns.asUInt else Cat(ns.asUInt(width - 1, 0), z.last)
        val znext = z.dropRight(1)
        val qnext = VecInit(cs.last +: q)

        if (stages contains depth) {
          val regznext = if (znext.isEmpty) znext else RegNext(VecInit(znext))
          nextLayer(
            RegNext(cs.last),
            regznext,
            RegNext(snext),
            RegNext(d),
            RegNext(qnext),
            depth + 1
          )
        } else
          nextLayer(cs.last, znext, snext, d, qnext, depth + 1)
      } else {
        (VecInit(q).asUInt, s)
      }
    }

    val s0 = z(2 * width - 1, width - 1)
    val d0 = Cat(0.U, d.asUInt)
    val z0 = z.asBools.dropRight(width + 1)

    nextLayer(1.B, z0, s0, d0, Seq(), 0)
  }

  val (q, s) = makeTree(io.z, io.d)
  io.Q := q
  io.S := s
}

class CSub extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val ctrl = Input(Bool())
    val cin = Input(Bool())

    val S = Output(Bool())
    val Cout = Output(Bool())
  })

  val (d, bout) = FS(io.a, io.b, io.cin)
  io.S := Mux(io.ctrl, d, io.a)
  io.Cout := bout
}

object CSub {
  def apply(a: Bool, b: Bool, ctrl: Bool, cin: Bool): (Bool, Bool) = {
    val mod = Module(new CSub)
    mod.io.a := a
    mod.io.b := b
    mod.io.ctrl := ctrl
    mod.io.cin := cin
    (mod.io.S, mod.io.Cout)
  }
}

class RestoringArrayDivider(val width: Int, val stages: Seq[Int])
    extends Module {
  val io = IO(new Bundle {
    val z = Input(UInt((2 * width).W)) // dividend
    val d = Input(UInt(width.W)) // divisor

    val Q = Output(UInt(width.W))
    val S = Output(UInt(width.W))
  })

  // println(s"Width $width...........")

  def makeTree(z: UInt, d: UInt): (UInt, UInt) = {
    /* nextLayer
     *
     * @param z    -> partial dividend (only the higher bits that are needed for layers after this one)
     * @param s    -> partial remainder after prev add/sub
     * @param d    -> divisor
     * @param q    -> quotient (each layer produces a new bit)
     */
    def nextLayer(
        z: Seq[Bool],
        s: UInt,
        d: UInt,
        q: Seq[Bool],
        depth: Int
    ): (UInt, UInt) = {

      // println(s"depth: $depth, z: ${z.size}, s: ${s.getWidth}, d: ${d.getWidth}, q: ${q.size}")
      if (depth < width) {
        val cs = Wire(Vec(width, Bool()))
        val ns = Wire(Vec(width, Bool()))
        val ctrl = Wire(Bool())

        for (i <- 0 until width) {
          val (ni, ci) = CSub(s(i), d(i), ctrl, if (i == 0) 0.B else cs(i - 1))
          ns(i) := ni
          cs(i) := ci
        }

        ctrl := s(width) | ~cs.last
        // ctrl := 0.U

        val snext =
          if (z.isEmpty) ns.asUInt else Cat(ns.asUInt, z.last)
        val znext = z.dropRight(1)
        val qnext = VecInit(ctrl +: q)

        if (stages contains depth) {
          val regznext = if (znext.isEmpty) znext else RegNext(VecInit(znext))
          nextLayer(
            regznext,
            RegNext(snext),
            RegNext(d),
            RegNext(qnext),
            depth + 1
          )
        } else
          nextLayer(znext, snext, d, qnext, depth + 1)
      } else {
        (VecInit(q).asUInt, s)
      }
    }

    val z0 = z.asBools.dropRight(width + 1)
    val s0 = z(2 * width - 1, width - 1)
    val d0 = d

    nextLayer(z0, s0, d0, Seq(), 0)
  }

  val (q, s) = makeTree(io.z, io.d)
  io.Q := q
  io.S := s
}
