package com.github.scytrowski.songo.model

final case class ObjectId private(data: HexString) {
  override def toString: String = data.toShortString
}

object ObjectId {
  val objectIdLength = 12

  // fixme: Add structure validation
  def fromHex(candidate: String): Either[String, ObjectId] =
    HexString
      .fromHex(candidate)
      .flatMap(validateHexString)
      .map(ObjectId.apply)

  def fromBytes(bytes: Array[Byte]): Either[String, ObjectId] =
    validateHexString(HexString.fromBytes(bytes)).map(ObjectId.apply)

  private def validateHexString(hex: HexString): Either[String, HexString] =
    if (hex.length == objectIdLength)
      Right(hex)
    else
      Left(s"Invalid hexadecimal representation of Object ID: ${hex.toString}")
}
