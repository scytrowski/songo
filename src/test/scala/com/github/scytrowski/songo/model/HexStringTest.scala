package com.github.scytrowski.songo.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class HexStringTest extends AnyFlatSpec with Matchers {
  behavior of "HexString.fromHex"

  it should "create HexString from valid hex" in {
    // given
    val candidate = "0x512deF"

    // when
    val Right(result) = HexString.fromHex(candidate)

    // then
    result.toList must contain theSameElementsAs List(81, 45, -17)
  }

  it should "create HexString from valid hex without '0x' prefix" in {
    // given
    val candidate = "123456"

    // when
    val Right(result) = HexString.fromHex(candidate)

    // then
    result.toList must contain theSameElementsAs List(18, 52, 86)
  }

  it should "fail on empty hex" in {
    HexString.fromHex("").isLeft mustBe true
  }

  it should "fail on hex string of odd length" in {
    HexString.fromHex("123").isLeft mustBe true
  }

  it should "fail on string with invalid character" in {
    HexString.fromHex("123X56").isLeft mustBe true
  }

  behavior of "HexString.toString"

  it should "render valid hex" in {
    // given
    val hexStr = HexString.fromBytes(Array[Byte](7, 17, 34))

    // when
    val hex = hexStr.toString

    // then
    hex mustBe "0x071122"
  }
}
