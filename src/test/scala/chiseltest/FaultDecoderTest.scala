import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import fixedpoint._

class FaultDecoderTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior.of("Testers2")

  it should "display decimal for fixedpoint" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new StaticModule((1.5).F(1.BP))) { c =>
        c.out.expect((1).F(1.BP))
      }
    }
    exc.getMessage should include("1.5 (3, 0x3)")
    exc.getMessage should include("expected=1.0 (2, 0x2)")
  }

  it should "display decimal for negative fixedpoint" in {
    val exc = intercept[exceptions.TestFailedException] {
      test(new StaticModule((-2.0).F(1.BP))) { c =>
        c.out.expect((-0.5).F(1.BP))
      }
    }
    exc.getMessage should include("-2.0 (-4, -0x4)")
    exc.getMessage should include("expected=-0.5 (-1, -0x1)")
  }
}
