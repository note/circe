package io.circe

import _root_.scodec.bits.{BitVector, ByteVector}

import scala.collection.immutable.StringOps
import scala.language.implicitConversions

package object scodec {
  @inline private implicit def augmentString(x: String): StringOps = new StringOps(x)

  implicit final val decodeBitVector: Decoder[BitVector] =
    Decoder.instance { c =>
      Decoder.decodeString(c) match {
        case Right(str) =>
          BitVector.fromBase64Descriptive(str.drop(1)) match {
            case Right(r) =>
              val numberOfCharsToIgnore = 8 - str.take(1).toByte
              val res = r.take(r.size - numberOfCharsToIgnore)
              Right(res)
            case Left(err) => Left(DecodingFailure(err, c.history))
          }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[BitVector]]
      }
    }

  implicit final val encodeBitVector: Encoder[BitVector] =
    Encoder.encodeString.contramap { bv =>
      val significantBits = if (bv.isEmpty) {
        0
      } else {
        val mod = bv.size % 8
        if (mod == 0) 8 else mod
      }
      significantBits.toByte + bv.toBase64
    }

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
