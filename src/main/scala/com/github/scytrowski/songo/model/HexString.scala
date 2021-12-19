package com.github.scytrowski.songo.model

import cats.data.NonEmptyList
import cats.instances.list._
import cats.instances.either._
import cats.syntax.traverse._

final case class HexString private(data: List[Byte]) {
  import HexString._

  override def toString: String = {
    if (data.nonEmpty) {
      val hexPart = data
        .map { byte =>
          val low = byte & 0xF
          val high = (byte >> 4) & 0xF
          s"${numberToHexDigit(high)}${numberToHexDigit(low)}"
        }
        .reduceLeft(_ + _)
      s"0x$hexPart"
    } else "0x0"
  }

  def toShortString: String = toString.stripPrefix("0x")

  def ++(other: HexString): HexString = combine(other)
  def combine(other: HexString): HexString = HexString(data ++ other.data)

  def toArray: Array[Byte] = data.toArray

  def toList: List[Byte] = data

  def length: Int = data.length

  def drop(n: Int): HexString = HexString(data.drop(n))
}

object HexString {
  def fromBytes(first: Byte, rest: Byte*): HexString = fromBytes(Array(first, rest:_*))

  def fromBytes(bytes: Array[Byte]): HexString = HexString(bytes.toList)

  def fromHex(candidate: String): Either[String, HexString] = {
    val candidateWithoutPrefix = candidate.stripPrefix("0x")

    NonEmptyList
      .fromList(candidateWithoutPrefix.toList)
      .filter(_.length % 2 == 0)
      .toRight("Hex string length must be a positive even integer")
      .map(fromHex)
      .flatten
  }

  private def fromHex(chars: NonEmptyList[Char]): Either[String, HexString] =
    chars
      .grouped(2)
      .toList
      .map(_.toList)
      .map(fromHexDigits)
      .sequence
      .map(HexString.apply)

  private def fromHexDigits(digits: List[Char]): Either[String, Byte] =
    for {
      first  <- hexDigitToInt(digits.head)
      second <- hexDigitToInt(digits(1))
    } yield ((first << 4) + second).toByte

  private def hexDigitToInt(digit: Char): Either[String, Int] =
    Option(digit).collect {
      case '0' => 0
      case '1' => 1
      case '2' => 2
      case '3' => 3
      case '4' => 4
      case '5' => 5
      case '6' => 6
      case '7' => 7
      case '8' => 8
      case '9' => 9
      case 'a' | 'A' => 10
      case 'b' | 'B' => 11
      case 'c' | 'C' => 12
      case 'd' | 'D' => 13
      case 'e' | 'E' => 14
      case 'f' | 'F' => 15
    }.toRight(s"Invalid hex digit: $digit")

  private def numberToHexDigit(number: Int): Char =
    number match {
      case 0 => '0'
      case 1 => '1'
      case 2 => '2'
      case 3 => '3'
      case 4 => '4'
      case 5 => '5'
      case 6 => '6'
      case 7 => '7'
      case 8 => '8'
      case 9 => '9'
      case 10 => 'A'
      case 11 => 'B'
      case 12 => 'C'
      case 13 => 'D'
      case 14 => 'E'
      case _  => 'F'
    }
}