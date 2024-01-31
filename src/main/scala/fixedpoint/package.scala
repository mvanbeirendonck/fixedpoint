// SPDX-License-Identifier: Apache-2.0

import chisel3._
import chisel3.experimental.Direction
import chisel3.reflect.DataMirror
import chiseltest.internal._
import chiseltest.UnpokeableException

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
      Utils.expectBits(x, value.litValue, message, Some(Utils.fixedToString(x.binaryPoint)))
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

  private object Utils {
    // Throw an exception if the value cannot fit into the signal.
    def ensureFits(signal: Bool, value: BigInt): Unit = {
      if (!(value == 0 || value == 1)) {
        throw new ChiselException(s"Value $value does not fit into the range of $signal (false/0 ... true/1)")
      }
    }

    // Throw an exception if the value cannot fit into the signal.
    def ensureFits(signal: UInt, value: BigInt): Unit = {
      signal.widthOption match {
        case Some(w) => ensureInRange(signal, value, 0, (BigInt(1) << w) - 1)
        case _       => // ignore
      }
    }

    // Throw an exception if the value cannot fit into the signal.
    def ensureFits(signal: SInt, value: BigInt): Unit = {
      signal.widthOption match {
        case Some(0) => // special case: 0-width SInts always represent 0
          ensureInRange(signal, value, 0, 0)
        case Some(w) =>
          val m = BigInt(1) << (w - 1)
          ensureInRange(signal, value, -m, m - 1)
        case _ => // ignore
      }
    }

    private def ensureInRange(signal: Data, value: BigInt, min: BigInt, max: BigInt): Unit = {
      if (value < min || value > max) {
        throw new ChiselException(s"Value $value does not fit into the range of $signal ($min ... $max)")
      }
    }

    // helps us work around the fact that signal.width is private!
    def getFirrtlWidth(signal: Data): chisel3.internal.firrtl.Width = signal.widthOption match {
      case Some(value) => chisel3.internal.firrtl.KnownWidth(value)
      case None        => chisel3.internal.firrtl.UnknownWidth()
    }

    // def boolBitsToString(bits: BigInt): String = (bits != 0).toString

    def fixedToString(binaryPoint: BinaryPoint): BigInt => String = {
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

    def enumToString(tpe: EnumType): BigInt => String = {
      def inner(bits: BigInt): String = {
        val fullName = chisel3.internaltest.EnumHelpers.valueToName(tpe, bits).getOrElse("???")
        // we only want to class and value name, not the package or enclosing class
        val noPackage = fullName.split('.').takeRight(2).mkString(".")
        val noEnclosingClass = noPackage.split('$').last
        noEnclosingClass
      }

      inner
    }

    def pokeBits(signal: Data, value: BigInt): Unit = {
      if (DataMirror.directionOf(signal) != Direction.Input) {
        throw new UnpokeableException("Can only poke inputs")
      }
      Context().backend.pokeBits(signal, value)
    }

    private def bigIntToHex(x: BigInt): String = {
      if (x < 0) {
        f"-0x${-x}%x"
      } else {
        f"0x$x%x"
      }
    }

    def expectEpsilon(
      signal:   Data,
      actual:   Double,
      expected: Double,
      epsilon:  Double,
      userMsg:  Option[() => String]
    ): Unit = {
      val ok = (actual - expected).abs < epsilon
      if (!ok) {
        val signalName = Context().backend.resolveName(signal)
        val msg = s"$signalName: ($actual - $expected).abs = ${(actual - expected).abs} >= eps=$epsilon"
        Utils.expectFailed(msg, userMsg)
      }
    }

    def expectEpsilon(
      signal:   Data,
      actual:   BigDecimal,
      expected: BigDecimal,
      epsilon:  BigDecimal,
      userMsg:  Option[() => String]
    ): Unit = {
      val ok = (actual - expected).abs < epsilon
      if (!ok) {
        val signalName = Context().backend.resolveName(signal)
        val msg = s"$signalName: ($actual - $expected).abs = ${(actual - expected).abs} >= eps=$epsilon"
        Utils.expectFailed(msg, userMsg)
      }
    }

    def expectBits(
      signal:   Data,
      expected: BigInt,
      msg:      Option[() => String],
      decode:   Option[BigInt => String]
    ): Unit = {
      val actual = Context().backend.peekBits(signal)
      if (expected != actual) {

        val (actualStr, expectedStr) = decode match {
          case Some(decode) =>
            (
              s"${decode(actual)} ($actual, ${bigIntToHex(actual)})",
              s"${decode(expected)} ($expected, ${bigIntToHex(expected)})"
            )
          case None =>
            (s"$actual (${bigIntToHex(actual)})", s"$expected (${bigIntToHex(expected)})")
        }
        val signalName = Context().backend.resolveName(signal)
        val message = s"$signalName=$actualStr did not equal expected=$expectedStr"
        expectFailed(message, msg)
      }
    }

    def expectFailed(message: String, userMsg: Option[() => String]): Unit = {
      val appendMsg = userMsg match {
        case Some(m) => s": ${m()}"
        case _       => ""
      }
      Context().env.signalExpectFailure(message + appendMsg)
    }
  }

}
