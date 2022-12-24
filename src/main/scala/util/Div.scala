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
  def apply(a: Bool, b: Bool, ctrl: Bool, cin: Bool) : (Bool, Bool) = {
    val mod = Module (new CAS)
    mod.io.a := a
    mod.io.b := b
    mod.io.ctrl := ctrl
    mod.io.cin := cin
    (mod.io.S, mod.io.Cout)
  }
}

class ArrayDivider(val width: Int, val stages: Seq[Int]) extends Module {
  val io = IO(new Bundle {
    val z = Input(UInt((2*width).W)) //dividend
    val d = Input(UInt(width.W)) //divisor

    val Q = Output(UInt((width+1).W))
  })

  def makeTree(z: UInt, d: UInt) : UInt = {
    /* nextLayer
     * 
     * @param ctrl -> add/sub control and cin from prev layer
     * @param z    -> partial dividend (only the lower bits that are needed for that layer and layers after that)
     * @param s    -> partial remainder after prev add/sub
     * @param d    -> divisor
     * @param q    -> quotient (each layer produces a new bit)
     */
    def nextLayer(ctrl: Bool, z: Seq[Bool], s: UInt, d: UInt, q: Seq[Bool], depth: Int) : UInt = {
      if (depth <= width) {
        val cs = Wire(Vec(width+1, Bool()))
        val ns = Wire(Vec(width+1, Bool()))

        for (i <- 0 to width) {
          val (ni, ci) = CAS(d(i), s(i), ctrl, if (i==0) ctrl else cs(i-1))
          ns(i) := ni
          cs(i) := ci
        }

        val snext = if (z.isEmpty) ns.asUInt else Cat(ns.asUInt(width-1, 0), z.last)
        val znext = z.dropRight(1)
        val qnext = VecInit(cs.last +: q)

        if (stages contains depth) {
          val regznext = if (znext.isEmpty) znext else RegNext(VecInit(znext))
          nextLayer(RegNext(cs.last), regznext, RegNext(snext), RegNext(d), RegNext(qnext), depth+1)
        } else
          nextLayer(cs.last, znext, snext, d, qnext, depth+1)
      } else {
        VecInit(q).asUInt
      }
    }

    val s0 = Cat(0.U, VecInit(z.asBools.drop(width)).asUInt)
    val d0 = Cat(0.U, d.asUInt)
    val z0 = z.asBools.dropRight(width)

    nextLayer(1.B, z0, s0, d0, Seq(), 0)
  }

  io.Q := makeTree(io.z, io.d)
}
