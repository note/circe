package io.circe

import _root_.scodec.bits.{ BitVector, ByteVector }
import scala.collection.immutable.StringOps

private[circe] trait BitVectorCodec {
  implicit final val decodeBitVector: Decoder[BitVector] =
    Decoder.instance { c =>
      Decoder.decodeString(c) match {
        case Right(str) =>
          val richStr = new StringOps(str)
          numberOfCharsToIgnore(richStr) match {
            case Some(toIgnore) =>
              BitVector.fromBase64Descriptive(richStr.drop(1)) match {
                case Left(err) => Left(DecodingFailure(err, c.history))
                case Right(r) =>
                  val res = r.take(r.size - toIgnore)
                  Right(res)
              }
            case None =>
              Left(DecodingFailure("Incorrect format of BitVector field", c.history))
          }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[BitVector]]
      }
    }

  /**
    * For serialization of BitVector we use base64. scodec's implementation of `toBase64` adds padding to 8 bits.
    * That's not desired in our case and to preserve original BitVector length we prefix base64 representation with
    * simple one byte long "header" which stands for number of bits of data in the last byte of base64 representation.
    *
    * Examples:
    * encodeBitVector(bin"101")
    * res: io.circe.Json = "3oA==" // starts with "3' because BitVector is of size 3
    *
    * encodeBitVector(bin"")
    * res: io.circe.Json = "0"
    *
    * encodeBitVector(bin"11001100")
    * res: io.circe.Json = "8zA=="
    *
    */
  implicit final val encodeBitVector: Encoder[BitVector] =
    Encoder.encodeString.contramap { bv =>
      significantBitsInLastByte(bv).toString + bv.toBase64
    }

  private def significantBitsInLastByte(bitVector: BitVector): Byte = {
    if (bitVector.isEmpty) {
      0
    } else {
      val mod = (bitVector.size % 8).toByte
      if (mod == 0) 8 else mod
    }
  }

  private def numberOfCharsToIgnore(input: StringOps): Option[Int] = {
    input.headOption.flatMap { head =>
      val digit = (new scala.runtime.RichChar(head)).asDigit
      if (digit >= 0 && digit <= 8) {
        Some (8 - digit)
      } else {
        None
      }
    }
  }
}

private[circe] trait ByteVectorCodec {
  implicit final val decodeByteVector: Decoder[ByteVector] =
    Decoder.instance { c =>
      Decoder.decodeString(c) match {
        case Right(str) =>
          ByteVector.fromBase64Descriptive(str) match {
            case r @ Right(_) => r.asInstanceOf[Decoder.Result[ByteVector]]
            case Left(err) => Left(DecodingFailure(err, c.history))
          }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[ByteVector]]
      }
    }

  implicit final val encodeByteVector: Encoder[ByteVector] =
    Encoder.encodeString.contramap(_.toBase64)
}

package object scodec extends ByteVectorCodec with BitVectorCodec
