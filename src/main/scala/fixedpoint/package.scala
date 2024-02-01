// SPDX-License-Identifier: Apache-2.0

import chisel3._
import chisel3.experimental.Direction
import chisel3.reflect.DataMirror
import chiseltest.internal._
import chiseltest.UnpokeableException
import chiseltest.Utils

package object fixedpoint extends FixedPoint.ImplicitsCls {

  implicit class fromSIntToFixedPoint(sInt: SInt) {
    def asFixedPoint(binaryPoint: BinaryPoint): FixedPoint = FixedPoint.fromData(binaryPoint, sInt)
  }

  implicit class fromUIntToFixedPoint(uInt: UInt) {
    def asFixedPoint(binaryPoint: BinaryPoint): FixedPoint = FixedPoint.fromData(binaryPoint, uInt.asSInt)
  }

  implicit class fromIntToBinaryPoint(int: Int) {
    def BP: BinaryPoint = BinaryPoint(int)
  }

  private def fixedToString(binaryPoint: BinaryPoint): BigInt => String = {
    def inner(bits: BigInt): String = {
      binaryPoint match {
        case KnownBinaryPoint(binaryPoint) =>
          val bpInteger = 1 << binaryPoint
          (bits.toFloat / bpInteger).toString
        case UnknownBinaryPoint => "[unknown binary point]"
      }
    }

    inner
  }

  /** allows access to chisel FixedPoint type signals with Scala native values */
  implicit class testableFixedPoint(x: FixedPoint) {
    private def asFixedPoint(value: Double):     FixedPoint = value.F(Utils.getFirrtlWidth(x), x.binaryPoint)
    private def asFixedPoint(value: BigDecimal): FixedPoint = value.F(Utils.getFirrtlWidth(x), x.binaryPoint)
    def poke(value: FixedPoint): Unit = {
      require(x.binaryPoint == value.binaryPoint, "binary point mismatch")
      Utils.pokeBits(x, value.litValue)
    }
    def poke(value: Double):     Unit = poke(asFixedPoint(value))
    def poke(value: BigDecimal): Unit = poke(asFixedPoint(value))
    private[fixedpoint] def expectInternal(value: FixedPoint, message: Option[() => String]): Unit = {
      require(x.binaryPoint == value.binaryPoint, "binary point mismatch")
      // for backwards compatibility reasons, we do not support epsilon when expect is called with the FixedPoint type.
      Utils.expectBits(x, value.litValue, message, Some(fixedToString(x.binaryPoint)))
    }
    def expect(value: FixedPoint): Unit = expectInternal(value, None)
    def expect(value: FixedPoint, message: => String): Unit = expectInternal(value, Some(() => message))
    private[fixedpoint] def expectInternal(expected: Double, epsilon: Double, userMsg: Option[() => String]): Unit = {
      Utils.expectEpsilon(x, peekDouble(), expected, epsilon, userMsg)
    }
    def expect(value: Double): Unit = expectInternal(value, epsilon = 0.01, None)
    def expect(value: Double, epsilon: Double): Unit = expectInternal(value, epsilon = epsilon, None)
    def expect(value: Double, message: => String): Unit =
      expectInternal(value, epsilon = 0.01, Some(() => message))
    def expect(value: Double, message: => String, epsilon: Double): Unit =
      expectInternal(value, epsilon = epsilon, Some(() => message))
    private[fixedpoint] def expectInternal(
      expected: BigDecimal,
      epsilon:  BigDecimal,
      userMsg:  Option[() => String]
    ): Unit = {
      Utils.expectEpsilon(x, peekBigDecimal(), expected, epsilon, userMsg)
    }
    def expect(value: BigDecimal): Unit = expectInternal(value, epsilon = 0.01, None)
    def expect(value: BigDecimal, epsilon: BigDecimal): Unit = expectInternal(value, epsilon = epsilon, None)
    def expect(value: BigDecimal, message: => String): Unit =
      expectInternal(value, epsilon = 0.01, Some(() => message))
    def expect(value: BigDecimal, message: => String, epsilon: BigDecimal): Unit =
      expectInternal(value, epsilon = epsilon, Some(() => message))
    def peek(): FixedPoint = {
      val multiplier = BigDecimal(2).pow(x.binaryPoint.get)
      (BigDecimal(Context().backend.peekBits(x)) / multiplier).F(x.binaryPoint)
    }
    def peekDouble(): Double = x.binaryPoint match {
      case KnownBinaryPoint(bp) => FixedPoint.toDouble(Context().backend.peekBits(x), bp)
      case _                    => throw new Exception("Cannot peekInterval with unknown binary point location")
    }
    def peekBigDecimal(): BigDecimal = x.binaryPoint match {
      case KnownBinaryPoint(bp) => FixedPoint.toBigDecimal(Context().backend.peekBits(x), bp)
      case _                    => throw new Exception("Cannot peekInterval with unknown binary point location")
    }
  }

}
