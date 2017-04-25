package io.circe.scodec

import _root_.scodec.bits.{BitVector, ByteVector}
import cats.Eq
import io.circe.Json
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

  "Codec[ByteVector]" should "return failure in case first character is not from range 0-8" in {
    val json = Json.fromString("amA==")
    assert(decodeBitVector.decodeJson(json).isLeft)

    val json2 = Json.fromString("9mA==")
    assert(decodeBitVector.decodeJson(json2).isLeft)
  }

  "Codec[ByteVector]" should "return failure in case input is an empty string" in {
    val json = Json.fromString("")
    assert(decodeBitVector.decodeJson(json).isLeft)
  }

  // this test shows that decoder is to some extend liberal
  // even though such input could not have been produced by BitVector encoder it's getting decoded to empty BitVector
  "Codec[ByteVector]" should "return empty BitVector in case contains only non-zero header" in {
    val json = Json.fromString("3")
    assert(decodeBitVector.decodeJson(json).right.get == BitVector.empty)
  }
}
