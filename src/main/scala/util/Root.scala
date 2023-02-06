package fpu

import chisel3._
import chisel3.util._

class SqRooter(val width: Int, val stages: Seq[Int]) extends Module {
  assert(width % 2 == 0)
  val io = IO(new Bundle {
    val z = Input(UInt(width.W)) // Radicand

    val Q = Output(UInt(width.W)) // Root
    val S = Output(UInt(width.W)) // Remainder
  })

  // println(s"width = $width .........")

  def makeTree(z: UInt): (UInt, UInt) = {
    def nextLayer(
        s: Seq[Bool],
        d: Seq[Bool],
        z: Seq[Bool],
        ctrl: Bool,
        q: Seq[Bool],
        depth: Int
    ): (UInt, UInt) = {
      // println(s"depth: $depth --> s: ${s.size}, d: ${d.size}, z: ${z.size}, q: ${q.size}")
      if (depth < width / 2) {
        val ns = Wire(Vec(2 * depth + 2, Bool()))
        val cs = Wire(Vec(2 * depth + 2, Bool()))

        for (i <- 0 until 2 * depth + 2) {
          val (ni, ci) = CAS(s(i), d(i), ctrl, if (i == 0) ctrl else cs(i - 1))
          ns(i) := ni
          cs(i) := ci
        }

        val ctrlnext = cs.last
        val qnext = VecInit(cs.last +: q)
        val znext = z.dropRight(2)
        val dnext = VecInit(z.takeRight(2) ++ ns)
        val snext = VecInit(1.B +: ~ctrlnext +: ctrlnext +: s.drop(2) :+ 0.B)

        if (stages contains depth) {
          val znextreg = if (znext.isEmpty) znext else RegNext(VecInit(znext))
          nextLayer(
            RegNext(snext),
            RegNext(dnext),
            znextreg,
            RegNext(ctrlnext),
            RegNext(qnext),
            depth + 1
          )
        } else {
          nextLayer(snext, dnext, znext, ctrlnext, qnext, depth + 1)
        }
      } else {
        (VecInit(q).asUInt, VecInit(d).asUInt)
      }
    }

    val s0 = Seq(1.B, 0.B)
    val d0 = z.asBools.takeRight(2)
    val z0 = z.asBools.dropRight(2)

    nextLayer(s0, d0, z0, 1.B, Seq(), 0)
  }

  val (q, s) = makeTree(io.z)
  io.Q := q
  io.S := s
}
