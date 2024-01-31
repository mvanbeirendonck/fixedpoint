import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import fixedpoint._

class ElementTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior.of("Testers2 with Element types")

  // TODO: automatically detect overflow conditions and error out

  it should "work with FixedPoint" in {
    test(new Module {
      val io = IO(new Bundle {
        val in1 = Input(FixedPoint(8.W, 2.BP))
        val in2 = Input(FixedPoint(8.W, 2.BP))
        val out = Output(FixedPoint(8.W, 2.BP))

        def expect(in1Val: FixedPoint, in2Val: FixedPoint, outVal: FixedPoint): Unit = {
          in1.poke(in1Val)
          in2.poke(in2Val)
          out.expect(outVal)
        }
      })
      io.out := io.in1 + io.in2
    }) { c =>
      c.io.expect(0.F(2.BP), 0.F(2.BP), 0.F(2.BP))
      c.io.expect(1.F(2.BP), 1.F(2.BP), 2.F(2.BP))
      c.io.expect(0.5.F(2.BP), 0.5.F(2.BP), 1.F(2.BP))
      c.io.expect(0.5.F(2.BP), -0.5.F(2.BP), 0.F(2.BP))

      // Overflow test, treating it as a 6-bit signed int
      c.io.expect(31.F(2.BP), 1.F(2.BP), -32.F(2.BP))
      c.io.expect(-32.F(2.BP), -1.F(2.BP), 31.F(2.BP))

      c.io.expect(31.F(2.BP), 31.F(2.BP), -2.F(2.BP))
      c.io.expect(-32.F(2.BP), -32.F(2.BP), 0.F(2.BP))

      // Overflow test with decimal component
      c.io.expect(31.75.F(2.BP), 31.75.F(2.BP), -0.5.F(2.BP))
      c.io.expect(31.75.F(2.BP), 0.25.F(2.BP), -32.F(2.BP))
    }
  }

  it should "peek on FixedPoint correctly" in {
    test(new PassthroughModule(new Bundle() {
      val d1 = FixedPoint(64.W, 0.BP)
      val d2 = FixedPoint(66.W, 2.BP)
    })) { c =>
      c.in.d1.poke(BigDecimal(Long.MaxValue).F(0.BP))
      c.out.d1.peek().litToBigDecimal should be(BigDecimal(Long.MaxValue))

      c.in.d1.poke(BigDecimal(Long.MinValue).F(0.BP))
      c.out.d1.peek().litToBigDecimal should be(BigDecimal(Long.MinValue))

      c.in.d1.poke(0.F(0.BP))
      c.out.d1.peek().litToBigDecimal should be(BigDecimal(0))

      c.in.d2.poke(BigDecimal(Long.MaxValue).F(2.BP))
      c.out.d2.peek().litToBigDecimal should be(BigDecimal(Long.MaxValue))

      c.in.d2.poke(BigDecimal(Long.MinValue).F(2.BP))
      c.out.d2.peek().litToBigDecimal should be(BigDecimal(Long.MinValue))

      c.in.d2.poke(0.F(2.BP))
      c.out.d2.peek().litToBigDecimal should be(BigDecimal(0))
    }
  }
}
