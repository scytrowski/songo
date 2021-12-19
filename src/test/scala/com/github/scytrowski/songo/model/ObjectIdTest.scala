package com.github.scytrowski.songo.model

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ObjectIdTest extends AnyFlatSpec with Matchers with EitherValues {
  behavior of "ObjectId.fromHex"

  it should "return Object ID created from valid hex" in {
    // given
    val hex = "507f1f77bcf86cd799439011"

    // when
    val result = ObjectId.fromHex(hex)

    // then
    result.value.toString.toLowerCase mustBe hex
  }

  it should "fail on invalid hex" in {

  }

  it should "fail on too short hex" in {
    ObjectId.fromHex("0x507f1f77bc6cd799439011").isLeft mustBe true
  }

  it should "fail on too long hex" in {
    ObjectId.fromHex("0x507f1f77bcf86cd79943901145e").isLeft mustBe true
  }
}
