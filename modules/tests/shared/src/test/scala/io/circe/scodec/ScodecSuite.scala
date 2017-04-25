package io.circe.scodec

import _root_.scodec.bits.{ BitVector, ByteVector }
import cats.Eq
import io.circe.{Decoder, Json}
import io.circe.testing.CodecTests
import io.circe.tests.CirceSuite
import org.scalacheck.Arbitrary

class ScodecSuite extends CirceSuite {
  implicit val arbitraryBitVector: Arbitrary[BitVector] =
    Arbitrary(Arbitrary.arbitrary[Iterable[Boolean]].map(BitVector.bits))

  implicit val arbitraryByteVector: Arbitrary[ByteVector] =
    Arbitrary(Arbitrary.arbitrary[Array[Byte]].map(ByteVector.view))

  implicit val eqBitVector: Eq[BitVector] =
    Eq.fromUniversalEquals

  implicit val eqByteVector: Eq[ByteVector] =
    Eq.fromUniversalEquals

  checkLaws("Codec[BitVector]", CodecTests[BitVector].codec)
  
  checkLaws("Codec[ByteVector]", CodecTests[ByteVector].codec)

  "A JSON number" should "not be decoded as a non-numeric type" in {
    val json = Json.fromString("amA==")
    assert(decodeBitVector.decodeJson(json).isLeft)
  }
}
