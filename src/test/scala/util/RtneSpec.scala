package fpu

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class RtneSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Rtne"

  it should "Round down" in {
    test(new Rtne(5, 2)) { c =>
      c.io.unrounded.poke("b10011".U)
      c.io.expIn.poke("b10".U)
      c.io.rounded.expect("b10".U)
      c.io.expOut.expect("b10".U)
    }
  }

  it should "Round down from mid" in {
    test(new Rtne(5, 2)) { c =>
      c.io.unrounded.poke("b10100".U)
      c.io.expIn.poke("b10".U)
      c.io.rounded.expect("b10".U)
      c.io.expOut.expect("b10".U)
    }
  }

  it should "Round up" in {
    test(new Rtne(5, 2)) { c =>
      c.io.unrounded.poke("b10101".U)
      c.io.expIn.poke("b10".U)
      c.io.rounded.expect("b11".U)
      c.io.expOut.expect("b10".U)
    }
  }

  it should "Round up from mid" in {
    test(new Rtne(5, 2)) { c =>
      c.io.unrounded.poke("b01100".U)
      c.io.expIn.poke("b10".U)
      c.io.rounded.expect("b10".U)
      c.io.expOut.expect("b10".U)
    }
  }

  it should "Round up with overflow" in {
    test(new Rtne(5, 2)) { c =>
      c.io.unrounded.poke("b11100".U)
      c.io.expIn.poke("b10".U)
      c.io.rounded.expect("b00".U)
      c.io.expOut.expect("b11".U)
    }
  }
}
