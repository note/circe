package io.circe

import cats.{ MonadError, SemigroupK }
import cats.data.{ Kleisli, NonEmptyList, NonEmptyVector, OneAnd, Validated, Xor }
import io.circe.export.Exported
import java.util.UUID
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.util.{ Failure, Success, Try }

trait Decoder[A] extends Serializable { self =>
  /**
   * Decode the given hcursor.
   */
  def apply(c: HCursor): Decoder.Result[A]

  private[circe] def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[A] = apply(c) match {
    case Right(a) => Validated.valid(a)
    case Left(e) => Validated.invalidNel(e)
  }

  /**
   * Decode the given acursor.
   */
  def tryDecode(c: ACursor): Decoder.Result[A] = if (c.succeeded) apply(c.any) else Left(
    DecodingFailure("Attempt to decode value on failed cursor", c.any.history)
  )

  def tryDecodeAccumulating(c: ACursor): AccumulatingDecoder.Result[A] =
    if (c.succeeded) decodeAccumulating(c.any) else Validated.invalidNel(
      DecodingFailure("Attempt to decode value on failed cursor", c.history)
    )

  /**
   * Decode the given [[Json]] value.
   */
  final def decodeJson(j: Json): Decoder.Result[A] = apply(HCursor.fromCursor(j.cursor))

  final def accumulating: AccumulatingDecoder[A] = AccumulatingDecoder.fromDecoder(self)

  /**
   * Map a function over this [[Decoder]].
   */
  final def map[B](f: A => B): Decoder[B] = new Decoder[B] {
    final def apply(c: HCursor): Decoder.Result[B] = self(c) match {
      case Right(a) => Right(f(a))
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[B]]
    }
    override def tryDecode(c: ACursor): Decoder.Result[B] = self.tryDecode(c) match {
      case Right(a) => Right(f(a))
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[B]]
    }
    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[B] =
      self.decodeAccumulating(c).map(f)
  }

  /**
   * Monadically bind a function over this [[Decoder]].
   */
  final def flatMap[B](f: A => Decoder[B]): Decoder[B] = new Decoder[B] {
    final def apply(c: HCursor): Decoder.Result[B] = self(c) match {
      case Right(a) => f(a)(c)
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[B]]
    }

    override def tryDecode(c: ACursor): Decoder.Result[B] = self.tryDecode(c) match {
      case Right(a) => f(a).tryDecode(c)
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[B]]
    }

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[B] =
      self.decodeAccumulating(c).andThen(result => f(result).decodeAccumulating(c))
  }

  /**
   * Create a new instance that handles any of this instance's errors with the
   * given function.
   *
   * Note that in the case of accumulating decoding, only the first error will
   * be used in recovery.
   */
  final def handleErrorWith(f: DecodingFailure => Decoder[A]): Decoder[A] = new Decoder[A] {
    final def apply(c: HCursor): Decoder.Result[A] =
      Decoder.resultInstance.handleErrorWith(self(c))(failure => f(failure)(c))

    override final def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[A] =
      AccumulatingDecoder.resultInstance.handleErrorWith(self.decodeAccumulating(c))(failures =>
        f(failures.head).decodeAccumulating(c)
      )
  }

  /**
   * Build a new instance with the specified error message.
   */
  final def withErrorMessage(message: String): Decoder[A] = new Decoder[A] {
    final def apply(c: HCursor): Decoder.Result[A] = self(c) match {
      case r @ Right(_) => r
      case Left(e) => Left(e.withMessage(message))
    }

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[A] =
      self.decodeAccumulating(c).leftMap(_.map(_.withMessage(message)))
  }

  /**
   * Build a new instance that fails if the condition does not hold.
   */
  final def validate(pred: HCursor => Boolean, message: => String): Decoder[A] = new Decoder[A] {
    final def apply(c: HCursor): Decoder.Result[A] =
      if (pred(c)) apply(c) else Left(DecodingFailure(message, c.history))
  }

  /**
   * Convert to a Kleisli arrow.
   */
  final def kleisli: Kleisli[Decoder.Result, HCursor, A] =
    Kleisli[Decoder.Result, HCursor, A](apply(_))

  /**
   * Run two decoders and return their results as a pair.
   */
  final def and[B](fb: Decoder[B]): Decoder[(A, B)] = new Decoder[(A, B)] {
    final def apply(c: HCursor): Decoder.Result[(A, B)] = Decoder.resultInstance.product(self(c), fb(c))
    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[(A, B)] =
      AccumulatingDecoder.resultInstance.product(self.decodeAccumulating(c), fb.decodeAccumulating(c))
  }

  /**
   * Choose the first succeeding decoder.
   */
  final def or[AA >: A](d: => Decoder[AA]): Decoder[AA] = new Decoder[AA] {
    final def apply(c: HCursor): Decoder.Result[AA] = self(c) match {
      case r @ Right(_) => r
      case Left(_) => d(c)
    }
  }

  /**
   * Run one or another decoder.
   */
  final def split[B](d: Decoder[B]): Either[HCursor, HCursor] => Decoder.Result[Either[A, B]] = _ match {
    case Left(c) => self(c) match {
      case Right(v) => Right(Left(v))
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[Either[A, B]]]
    }
    case Right(c) => d(c) match {
      case Right(v) => Right(Right(v))
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[Either[A, B]]]
    }
  }

  /**
   * Run two decoders.
   */
  final def product[B](x: Decoder[B]): (HCursor, HCursor) => Decoder.Result[(A, B)] = (a1, a2) =>
    Decoder.resultInstance.product(self(a1), x(a2))

  /**
   * Create a new decoder that performs some operation on the incoming JSON before decoding.
   */
  final def prepare(f: HCursor => ACursor): Decoder[A] = new Decoder[A] {
    final def apply(c: HCursor): Decoder.Result[A] = self.tryDecode(f(c))
    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[A] =
      self.tryDecodeAccumulating(f(c))
  }

  /**
   * Create a new decoder that performs some operation on the result if this one succeeds.
   *
   * @param f a function returning either a value or an error message
   */
  final def emap[B](f: A => Either[String, B]): Decoder[B] = new Decoder[B] {
    final def apply(c: HCursor): Decoder.Result[B] =
      self(c) match {
        case Right(a) => f(a) match {
          case r @ Right(_) => r.asInstanceOf[Decoder.Result[B]]
          case Left(message) => Left(DecodingFailure(message, c.history))
        }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[B]]
      }
  }
  /**
   * Create a new decoder that performs some operation on the result if this one succeeds.
   *
   * @param f a function returning either a value or an error message
   */
  final def emapTry[B](f: A => Try[B]): Decoder[B] = new Decoder[B] {
    final def apply(c: HCursor): Decoder.Result[B] =
      self(c) match {
        case Right(a) => f(a) match {
          case Success(b) => Right(b)
          case Failure(t) => Left(DecodingFailure.fromThrowable(t, c.history))
        }
        case l @ Left(_) => l.asInstanceOf[Decoder.Result[B]]
      }
  }
}

/**
 * Utilities and instances for [[Decoder]].
 *
 * @groupname Utilities Miscellaneous utilities
 * @groupprio Utilities 0
 *
 * @groupname Decoding Decoder instances
 * @groupprio Decoding 2
 *
 * @groupname Disjunction Disjunction instances
 * @groupdesc Disjunction Instance creation methods for disjunction-like types. Note that these
 * instances are not implicit, since they require non-obvious decisions about the names of the
 * discriminators. If you want instances for these types you can include the following import in
 * your program:
 * {{{
 *   import io.circe.disjunctionCodecs._
 * }}}
 * @groupprio Disjunction 3
 *
 * @groupname Instances Type class instances
 * @groupprio Instances 4
 *
 * @groupname Tuple Tuple instances
 * @groupprio Tuple 5
 *
 * @groupname Product Case class and other product instances
 * @groupprio Product 6
 *
 * @author Travis Brown
 */
final object Decoder extends TupleDecoders with ProductDecoders with LowPriorityDecoders {
  import Json._

  type Result[A] = Either[DecodingFailure, A]

  val resultInstance: MonadError[Result, DecodingFailure] = new MonadError[Result, DecodingFailure] {
    final def pure[A](x: A): Decoder.Result[A] = Right(x)
    override final def map[A, B](fa: Decoder.Result[A])(f: A => B): Decoder.Result[B] = fa match {
      case Right(a) => Right(f(a))
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[B]]
    }
    final def flatMap[A, B](fa: Decoder.Result[A])(f: A => Decoder.Result[B]): Decoder.Result[B] = fa match {
      case Right(a) => f(a)
      case l @ Left(_) => l.asInstanceOf[Decoder.Result[B]]
    }
    final def raiseError[A](e: DecodingFailure): Decoder.Result[A] = Left(e)
    final def handleErrorWith[A](fa: Decoder.Result[A])(f: DecodingFailure => Decoder.Result[A]): Decoder.Result[A] =
      fa match {
        case r @ Right(_) => r
        case Left(e) => f(e)
      }
  }

  /**
   * Return an instance for a given type.
   *
   * @group Utilities
   */
  final def apply[A](implicit instance: Decoder[A]): Decoder[A] = instance

  /**
   * Create a decoder that always returns a single value, useful with some flatMap situations
   */
  final def const[A](a: A): Decoder[A] = new Decoder[A] {
    final def apply(c: HCursor): Result[A] = Right(a)
    final override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[A] =
      Validated.valid(a)
  }

  /**
   * Construct an instance from a function.
   *
   * @group Utilities
   */
  final def instance[A](f: HCursor => Result[A]): Decoder[A] = new Decoder[A] {
    final def apply(c: HCursor): Result[A] = f(c)
  }

  /**
   * This is for easier interop with code that already returns Try. You should
   * prefer instance for any new code.
   */
  final def instanceTry[A](f: HCursor => Try[A]): Decoder[A] = new Decoder[A] {
    final def apply(c: HCursor): Result[A] = f(c) match {
      case Success(a) => Right(a)
      case Failure(t) => Left(DecodingFailure.fromThrowable(t, c.history))
    }
  }

  /**
   * Construct an instance from a function that may reattempt on failure.
   *
   * @group Utilities
   */
  final def withReattempt[A](f: ACursor => Result[A]): Decoder[A] = new Decoder[A] {
    final def apply(c: HCursor): Result[A] = tryDecode(c.acursor)

    override def tryDecode(c: ACursor): Decoder.Result[A] = f(c)

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[A] =
      tryDecodeAccumulating(c.acursor)

    override def tryDecodeAccumulating(c: ACursor): AccumulatingDecoder.Result[A] = f(c) match {
      case Right(v) => Validated.valid(v)
      case Left(e) => Validated.invalidNel(e)
    }
  }

  /**
   * Construct an instance that always fails with the given [[DecodingFailure]].
   *
   * @group Utilities
   */
  final def failed[A](failure: DecodingFailure): Decoder[A] = new Decoder[A] {
    final def apply(c: HCursor): Result[A] = Left(failure)
    override final def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[A] =
      Validated.invalidNel(failure)
  }

  /**
   * Construct an instance that always fails with the given error message.
   *
   * @group Utilities
   */
  final def failedWithMessage[A](message: String): Decoder[A] = failed(DecodingFailure(message, Nil))

  /**
   * @group Decoding
   */
  implicit final val decodeHCursor: Decoder[HCursor] = new Decoder[HCursor] {
    final def apply(c: HCursor): Result[HCursor] = Right(c)
  }

  /**
   * @group Decoding
   */
  implicit final val decodeJson: Decoder[Json] = new Decoder[Json] {
    final def apply(c: HCursor): Result[Json] = Right(c.focus)
  }

  /**
   * @group Decoding
   */
  implicit final val decodeJsonObject: Decoder[JsonObject] = new Decoder[JsonObject] {
    final def apply(c: HCursor): Result[JsonObject] = c.focus.asObject match {
      case Some(v) => Right(v)
      case None => Left(DecodingFailure("JsonObject", c.history))
    }
  }

  /**
   * @group Decoding
   */
  implicit final val decodeJsonNumber: Decoder[JsonNumber] = new Decoder[JsonNumber] {
    final def apply(c: HCursor): Result[JsonNumber] = c.focus.asNumber match {
      case Some(v) => Right(v)
      case None => Left(DecodingFailure("JsonNumber", c.history))
    }
  }

  /**
   * @group Decoding
   */
  implicit final val decodeString: Decoder[String] = new Decoder[String] {
    final def apply(c: HCursor): Result[String] = c.focus match {
      case JString(string) => Right(string)
      case _ => Left(DecodingFailure("String", c.history))
    }
  }

  /**
   * @group Decoding
   */
  implicit final val decodeUnit: Decoder[Unit] = new Decoder[Unit] {
    final def apply(c: HCursor): Result[Unit] = c.focus match {
      case JNull => Right(())
      case JObject(obj) if obj.isEmpty => Right(())
      case JArray(arr) if arr.isEmpty => Right(())
      case _ => Left(DecodingFailure("Unit", c.history))
    }
  }

  /**
   * @group Decoding
   */
  implicit final val decodeBoolean: Decoder[Boolean] = new Decoder[Boolean] {
    final def apply(c: HCursor): Result[Boolean] = c.focus match {
      case JBoolean(b) => Right(b)
      case _ => Left(DecodingFailure("Boolean", c.history))
    }
  }

  /**
   * @group Decoding
   */
  implicit final val decodeChar: Decoder[Char] = new Decoder[Char] {
    final def apply(c: HCursor): Result[Char] = c.focus match {
      case JString(string) if string.length == 1 => Right(string.charAt(0))
      case _ => Left(DecodingFailure("Char", c.history))
    }
  }

  /**
   * Decode a JSON value into a [[scala.Float]].
   *
   * See [[decodeDouble]] for discussion of the approach taken for floating-point decoding.
   *
   * @group Decoding
   */
  implicit final val decodeFloat: Decoder[Float] = new Decoder[Float] {
    private[this] def fail(c: HCursor): Result[Float] = Left(DecodingFailure("Float", c.history))

    final def apply(c: HCursor): Result[Float] = c.focus match {
      case JNull => Right(Float.NaN)
      case JNumber(number) => Right(number.toDouble.toFloat)
      case JString(string) => JsonNumber.fromString(string).map(_.toDouble.toFloat) match {
        case Some(v) => Right(v)
        case None => fail(c)
      }
      case _ => fail(c)
    }
  }

  /**
   * Decode a JSON value into a [[scala.Double]].
   *
   * Unlike the integral decoders provided here, this decoder will accept values that are too large
   * to be represented and will return them as `PositiveInfinity` or `NegativeInfinity`, and it may
   * lose precision.
   *
   * @group Decoding
   */
  implicit final val decodeDouble: Decoder[Double] = new Decoder[Double] {
    private[this] def fail(c: HCursor): Result[Double] = Left(DecodingFailure("Double", c.history))

    final def apply(c: HCursor): Result[Double] = c.focus match {
      case JNull => Right(Double.NaN)
      case JNumber(number) => Right(number.toDouble)
      case JString(string) => JsonNumber.fromString(string).map(_.toDouble) match {
        case Some(v) => Right(v)
        case None => fail(c)
      }
      case _ => fail(c)
    }
  }

  /**
   * Decode a JSON value into a [[scala.Byte]].
   *
   * See [[decodeLong]] for discussion of the approach taken for integral decoding.
   *
   * @group Decoding
   */
  implicit final val decodeByte: Decoder[Byte] = new Decoder[Byte] {
    private[this] def fail(c: HCursor): Result[Byte] = Left(DecodingFailure("Byte", c.history))

    final def apply(c: HCursor): Result[Byte] = c.focus match {
      case JNumber(number) => number.toByte match {
        case Some(v) => Right(v)
        case None => fail(c)
      }
      case JString(string) => try Right(string.toByte) catch {
        case _: NumberFormatException => fail(c)
      }
      case _ => fail(c)
    }
  }

  /**
   * Decode a JSON value into a [[scala.Short]].
   *
   * See [[decodeLong]] for discussion of the approach taken for integral decoding.
   *
   * @group Decoding
   */
  implicit final val decodeShort: Decoder[Short] = new Decoder[Short] {
    private[this] def fail(c: HCursor): Result[Short] = Left(DecodingFailure("Short", c.history))

    final def apply(c: HCursor): Result[Short] = c.focus match {
      case JNumber(number) => number.toShort match {
        case Some(v) => Right(v)
        case None => fail(c)
      }
      case JString(string) => try Right(string.toShort) catch {
        case _: NumberFormatException => fail(c)
      }
      case _ => fail(c)
    }
  }

  /**
   * Decode a JSON value into a [[scala.Int]].
   *
   * See [[decodeLong]] for discussion of the approach taken for integral decoding.
   *
   * @group Decoding
   */
  implicit final val decodeInt: Decoder[Int] = new Decoder[Int] {
    private[this] def fail(c: HCursor): Result[Int] = Left(DecodingFailure("Int", c.history))

    final def apply(c: HCursor): Result[Int] = c.focus match {
      case JNumber(number) => number.toInt match {
        case Some(v) => Right(v)
        case None => fail(c)
      }
      case JString(string) => try Right(string.toInt) catch {
        case _: NumberFormatException => fail(c)
      }
      case _ => fail(c)
    }
  }

  /**
   * Decode a JSON value into a [[scala.Long]].
   *
   * Decoding will fail if the value doesn't represent a whole number within the range of the target
   * type (although it can have a decimal part: e.g. `10.0` will be successfully decoded, but
   * `10.01` will not). If the value is a JSON string, the decoder will attempt to parse it as a
   * number.
   *
   * @group Decoding
   */
  implicit final val decodeLong: Decoder[Long] = new Decoder[Long] {
    private[this] def fail(c: HCursor): Result[Long] = Left(DecodingFailure("Long", c.history))

    final def apply(c: HCursor): Result[Long] = c.focus match {
      case JNumber(number) => number.toLong match {
        case Some(v) => Right(v)
        case None => fail(c)
      }
      case JString(string) => try Right(string.toLong) catch {
        case _: NumberFormatException => fail(c)
      }
      case _ => fail(c)
    }
  }

  /**
   * Decode a JSON value into a [[scala.math.BigInt]].
   *
   * Note that decoding will fail if the number has a large number of digits (the limit is currently
   * `1 << 18`, or around a quarter million). Larger numbers can be decoded by mapping over a
   * [[scala.math.BigDecimal]], but be aware that the conversion to the integral form can be
   * computationally expensive.
   *
   * @group Decoding
   */
  implicit final val decodeBigInt: Decoder[BigInt] = new Decoder[BigInt] {
    private[this] def fail(c: HCursor): Result[BigInt] = Left(DecodingFailure("BigInt", c.history))

    final def apply(c: HCursor): Result[BigInt] = c.focus match {
      case JNumber(number) => number.toBigInt match {
        case Some(v) => Right(v)
        case None => fail(c)
      }
      case JString(string) => try Right(BigInt(string)) catch {
        case _: NumberFormatException => fail(c)
      }
      case _ => fail(c)
    }
  }

  /**
   * Decode a JSON value into a [[scala.math.BigDecimal]].
   *
   * Note that decoding will fail on some very large values that could in principle be represented
   * as `BigDecimal`s (specifically if the `scale` is out of the range of `scala.Int` when the
   * `unscaledValue` is adjusted to have no trailing zeros). These large values can, however, be
   * round-tripped through `JsonNumber`, so you may wish to use [[decodeJsonNumber]] in these cases.
   *
   * Also note that because `scala.scalajs.js.JSON` parses JSON numbers into a floating point
   * representation, decoding a JSON number into a `BigDecimal` on Scala.js may lose precision.
   *
   * @group Decoding
   */
  implicit final val decodeBigDecimal: Decoder[BigDecimal] = new Decoder[BigDecimal] {
    private[this] def fail(c: HCursor): Result[BigDecimal] = Left(DecodingFailure("BigDecimal", c.history))

    final def apply(c: HCursor): Result[BigDecimal] = c.focus match {
      case JNumber(number) => number.toBigDecimal match {
        case Some(v) => Right(v)
        case None => fail(c)
      }
      case JString(string) => try Right(BigDecimal(string)) catch {
        case _: NumberFormatException => fail(c)
      }
      case _ => fail(c)
    }
  }

  /**
   * @group Decoding
   */
  implicit final val decodeUUID: Decoder[UUID] = new Decoder[UUID] {
    private[this] def fail(c: HCursor): Result[UUID] = Left(DecodingFailure("UUID", c.history))

    final def apply(c: HCursor): Result[UUID] = c.focus match {
      case JString(string) if string.length == 36 => try Right(UUID.fromString(string)) catch {
        case _: IllegalArgumentException => fail(c)
      }
      case _ => fail(c)
    }
  }

  /**
   * @group Decoding
   */
  implicit final def decodeCanBuildFrom[A, C[_]](implicit
    d: Decoder[A],
    cbf: CanBuildFrom[Nothing, A, C[A]]
  ): Decoder[C[A]] = new Decoder[C[A]] {
    final def apply(c: HCursor): Decoder.Result[C[A]] = {
      val arrayCursor = c.downArray

      if (arrayCursor.succeeded) {
        arrayCursor.any.traverseDecode(cbf.apply)(
          _.right,
          (acc, hcursor) => hcursor.as[A] match {
            case Right(a) => Right(acc += a)
            case l @ Left(_) => l.asInstanceOf[Decoder.Result[Builder[A, C[A]]]]
          }
        ) match {
          case Right(builder) => Right(builder.result)
          case l @ Left(_) => l.asInstanceOf[Decoder.Result[C[A]]]
        }
      } else if (c.focus.isArray)
          Right(cbf.apply.result)
        else
          Left(DecodingFailure("CanBuildFrom for A", c.history))
    }

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[C[A]] = {
      val arrayCursor = c.downArray

      if (arrayCursor.succeeded) {
        arrayCursor.any.traverseDecodeAccumulating(Validated.valid(cbf.apply))(
          _.right,
          (acc, hcursor) => AccumulatingDecoder.resultInstance.map2(acc, d.decodeAccumulating(hcursor))(_ += _)
        ).map(_.result)
      } else if (c.focus.isArray)
          Validated.valid(cbf.apply.result)
        else
          Validated.invalidNel(DecodingFailure("CanBuildFrom for A", c.history))
    }
  }

  private[this] final val rightNone: Either[DecodingFailure, Option[Nothing]] = Right(None)

  /**
   * @group Decoding
   */
  implicit final def decodeOption[A](implicit d: Decoder[A]): Decoder[Option[A]] =
    withReattempt(c =>
      if (c.succeeded) {
        if (c.any.focus.isNull) rightNone else d(c.any) match {
          case Right(a) => Right(Some(a))
          case Left(df) if df.history.isEmpty => rightNone
          case Left(df) => Left(df)
        }
      } else if (!c.history.takeWhile(_.failed).exists(_.incorrectFocus)) rightNone else {
        Left(DecodingFailure("[A]Option[A]", c.history))
      }
    )

  /**
   * @group Decoding
   */
  implicit final def decodeSome[A](implicit d: Decoder[A]): Decoder[Some[A]] = d.map(Some(_))

  /**
   * @group Decoding
   */
  implicit final val decodeNone: Decoder[None.type] = new Decoder[None.type] {
    final def apply(c: HCursor): Result[None.type] = if (c.focus.isNull) Right(None) else {
      Left(DecodingFailure("None", c.history))
    }
  }


  /**
   * @group Decoding
   */
  implicit final def decodeMapLike[M[K, +V] <: Map[K, V], K, V](implicit
    dk: KeyDecoder[K],
    dv: Decoder[V],
    cbf: CanBuildFrom[Nothing, (K, V), M[K, V]]
  ): Decoder[M[K, V]] = new Decoder[M[K, V]] {
    private[this] def failure(c: HCursor): DecodingFailure = DecodingFailure("[K, V]Map[K, V]", c.history)

    def apply(c: HCursor): Result[M[K, V]] = c.fields match {
      case None => Left[DecodingFailure, M[K, V]](failure(c))
      case Some(fields) =>
        val builder = cbf()
        spinResult(fields, c, builder) match {
          case None => Right(builder.result())
          case Some(error) => Left(error)
        }
    }

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[M[K, V]] =
      c.fields match {
        case None => Validated.invalidNel(failure(c))
        case Some(fields) =>
          val builder = cbf()
          spinAccumulating(fields, c, builder, false, List.newBuilder[DecodingFailure]) match {
            case Nil => Validated.valid(builder.result())
            case error :: errors => Validated.invalid(NonEmptyList(error, errors))
          }
      }

    @scala.annotation.tailrec
    private[this] def spinResult(
      fields: List[String],
      c: HCursor,
      builder: Builder[(K, V), M[K, V]]
    ): Option[DecodingFailure] = fields match {
      case Nil => None
      case h :: t =>
        val atH = c.downField(h)

        atH.as(dv) match {
          case Left(error) => Some(error)
          case Right(value) => dk(h) match {
            case None => Some(failure(atH.any))
            case Some(k) =>
              builder += (k -> value)
              spinResult(t, c, builder)
          }
      }
    }

    @scala.annotation.tailrec
    private[this] def spinAccumulating(
      fields: List[String],
      c: HCursor,
      builder: Builder[(K, V), M[K, V]],
      failed: Boolean,
      errors: Builder[DecodingFailure, List[DecodingFailure]]
    ): List[DecodingFailure] = fields match {
      case Nil => errors.result
      case h :: t =>
        val atH = c.downField(h)

        (atH.as(dv), dk(h)) match {
          case (Left(error), _) => spinAccumulating(t, c, builder, true, errors += error)
          case (_, None) => spinAccumulating(t, c, builder, true, errors += failure(atH.any))
          case (Right(value), Some(k)) =>
            if (!failed) {
              builder += (k -> value)
            } else ()

            spinAccumulating(t, c, builder, failed, errors)
        }
    }
  }

  /**
   * @group Decoding
   */
  implicit final def decodeSet[A: Decoder]: Decoder[Set[A]] =
    decodeCanBuildFrom[A, List].map(_.toSet).withErrorMessage("[A]Set[A]")

  /**
   * @group Decoding
   */
  implicit final def decodeOneAnd[A, C[_]](implicit
    da: Decoder[A],
    cbf: CanBuildFrom[Nothing, A, C[A]]
  ): Decoder[OneAnd[C, A]] = new Decoder[OneAnd[C, A]] {
    def apply(c: HCursor): Result[OneAnd[C, A]] = {
      val arr = c.downArray

      da.tryDecode(arr) match {
        case Right(head) => decodeCanBuildFrom[A, C].tryDecode(arr.delete) match {
          case Right(tail) => Right(OneAnd(head, tail))
          case l @ Left(_) => l.asInstanceOf[Result[OneAnd[C, A]]]
        }
        case l @ Left(_) => l.asInstanceOf[Result[OneAnd[C, A]]]
      }
    }

    override private[circe] def decodeAccumulating(
      c: HCursor
    ): AccumulatingDecoder.Result[OneAnd[C, A]] = {
      val arr = c.downArray
      val head = da.tryDecodeAccumulating(arr)
      val tail = decodeCanBuildFrom[A, C].tryDecodeAccumulating(arr.delete)
      tail.ap(head.map(h => (t: C[A]) => OneAnd(h, t)))
    }
  }

  /**
   * @group Decoding
   */
  implicit final def decodeNonEmptyList[A](implicit da: Decoder[A]): Decoder[NonEmptyList[A]] =
    new Decoder[NonEmptyList[A]] {
      def apply(c: HCursor): Result[NonEmptyList[A]] = {
        val arr = c.downArray

        da.tryDecode(arr) match {
          case Right(head) => decodeCanBuildFrom[A, List].tryDecode(arr.delete) match {
            case Right(tail) => Right(NonEmptyList(head, tail))
            case l @ Left(_) => l.asInstanceOf[Result[NonEmptyList[A]]]
          }
          case l @ Left(_) => l.asInstanceOf[Result[NonEmptyList[A]]]
        }
      }

      override private[circe] def decodeAccumulating(
        c: HCursor
      ): AccumulatingDecoder.Result[NonEmptyList[A]] = {
        val arr = c.downArray
        val head = da.tryDecodeAccumulating(arr)
        val tail = decodeCanBuildFrom[A, List].tryDecodeAccumulating(arr.delete)
        tail.ap(head.map(h => (t: List[A]) => NonEmptyList(h, t)))
      }
    }

  /**
   * @group Decoding
   */
  implicit final def decodeNonEmptyVector[A](implicit da: Decoder[A]): Decoder[NonEmptyVector[A]] =
    new Decoder[NonEmptyVector[A]] {
      def apply(c: HCursor): Result[NonEmptyVector[A]] = {
        val arr = c.downArray

        da.tryDecode(arr) match {
          case Right(head) => decodeCanBuildFrom[A, Vector].tryDecode(arr.delete) match {
            case Right(tail) => Right(NonEmptyVector(head, tail))
            case l @ Left(_) => l.asInstanceOf[Result[NonEmptyVector[A]]]
          }
          case l @ Left(_) => l.asInstanceOf[Result[NonEmptyVector[A]]]
        }
      }

      override private[circe] def decodeAccumulating(
        c: HCursor
      ): AccumulatingDecoder.Result[NonEmptyVector[A]] = {
        val arr = c.downArray
        val head = da.tryDecodeAccumulating(arr)
        val tail = decodeCanBuildFrom[A, Vector].tryDecodeAccumulating(arr.delete)
        tail.ap(head.map(h => (t: Vector[A]) => NonEmptyVector(h, t)))
      }
    }

  /**
   * @group Disjunction
   */
  final def decodeXor[A, B](leftKey: String, rightKey: String)(implicit
    da: Decoder[A],
    db: Decoder[B]
  ): Decoder[Xor[A, B]] = new Decoder[Xor[A, B]] {
    final def apply(c: HCursor): Result[Xor[A, B]] = {
      val l = c.downField(leftKey)
      val r = c.downField(rightKey)

      if (l.succeeded && !r.succeeded) {
        da(l.any) match {
          case Right(v) => Right(Xor.Left(v))
          case l @ Left(_) => l.asInstanceOf[Result[Xor[A, B]]]
        }
      } else if (!l.succeeded && r.succeeded) {
        db(r.any) match {
          case Right(v) => Right(Xor.Right(v))
          case l @ Left(_) => l.asInstanceOf[Result[Xor[A, B]]]
        }
      } else Left(DecodingFailure("[A, B]Xor[A, B]", c.history))
    }
  }

  /**
   * @group Disjunction
   */
  final def decodeEither[A, B](leftKey: String, rightKey: String)(implicit
    da: Decoder[A],
    db: Decoder[B]
  ): Decoder[Either[A, B]] =
    decodeXor[A, B](leftKey, rightKey).map(_.toEither).withErrorMessage("[A, B]Either[A, B]")

  /**
   * @group Disjunction
   */
  final def decodeValidated[E, A](failureKey: String, successKey: String)(implicit
    de: Decoder[E],
    da: Decoder[A]
  ): Decoder[Validated[E, A]] =
    decodeXor[E, A](
      failureKey,
      successKey
    ).map(_.toValidated).withErrorMessage("[E, A]Validated[E, A]")

  /**
   * @group Instances
   */
  implicit final val monadInstances: SemigroupK[Decoder] with MonadError[Decoder, DecodingFailure] =
    new SemigroupK[Decoder] with MonadError[Decoder, DecodingFailure] {
      final def combineK[A](x: Decoder[A], y: Decoder[A]): Decoder[A] = x.or(y)
      final def pure[A](a: A): Decoder[A] = const(a)
      override final def map[A, B](fa: Decoder[A])(f: A => B): Decoder[B] = fa.map(f)
      override final def product[A, B](fa: Decoder[A], fb: Decoder[B]): Decoder[(A, B)] = fa.and(fb)
      final def flatMap[A, B](fa: Decoder[A])(f: A => Decoder[B]): Decoder[B] = fa.flatMap(f)

      final def raiseError[A](e: DecodingFailure): Decoder[A] = Decoder.failed(e)
      final def handleErrorWith[A](fa: Decoder[A])(f: DecodingFailure => Decoder[A]): Decoder[A] = fa.handleErrorWith(f)
    }

  /**
    * @group Enumeration
    * {{{
    *   object WeekDay extends Enumeration { ... }
    *   implicit val weekDayDecoder = Decoder.enumDecoder(WeekDay)
    * }}}
    */
  final def enumDecoder[E <: Enumeration](enum: E): Decoder[E#Value] =
    Decoder.decodeString.flatMap { str =>
      Decoder.instanceTry { _ =>
        Try(enum.withName(str))
      }
    }
}

private[circe] trait LowPriorityDecoders {
  implicit def importedDecoder[A](implicit exported: Exported[Decoder[A]]): Decoder[A] = exported.instance
}
