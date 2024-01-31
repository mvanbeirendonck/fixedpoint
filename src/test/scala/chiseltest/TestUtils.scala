import chisel3._
import chisel3.util._

import scala.collection.immutable.ListMap

class StaticModule[T <: Data](ioLit: T) extends Module {
  val out = IO(Output(chiselTypeOf(ioLit)))
  out := ioLit
}

class PassthroughModule[T <: Data](ioType: T) extends Module {
  val in = IO(Input(ioType))
  val out = IO(Output(ioType))
  out := in
}
