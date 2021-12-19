package com.github.scytrowski.songo

import com.github.scytrowski.songo.model.ObjectId

sealed trait MongoValue

object MongoValue {
  case object MongoNull extends MongoNull
  sealed trait MongoNull extends MongoValue

  final case class MongoBoolean(value: Boolean) extends MongoValue
  final case class MongoInt(value: Int) extends MongoValue
  final case class MongoLong(value: Long) extends MongoValue
  final case class MongoDouble(value: Double) extends MongoValue
  final case class MongoString(value: String) extends MongoValue
  final case class MongoObjectId(value: ObjectId) extends MongoValue

  final case class MongoArray(values: List[MongoValue]) extends MongoValue {
    def append(value: MongoValue): MongoArray = MongoArray(values :+ value)

    def ++(other: MongoArray): MongoArray = merge(other)
    def merge(other: MongoArray): MongoArray = MongoArray(values ++ other.values)
  }

  object MongoArray {
    def apply(values: MongoValue*): MongoArray = MongoArray(values.toList)

    val empty: MongoArray = MongoArray()
  }

  final case class MongoObject(values: Map[String, MongoValue]) extends MongoValue {
    def updated(key: String, value: MongoValue): MongoObject = MongoObject(values.updated(key, value))

    def ++(other: MongoObject): MongoObject = merge(other)
    def merge(other: MongoObject): MongoObject = MongoObject(values ++ other.values)
  }

  object MongoObject {
    def apply(entries: (String, MongoValue)*): MongoObject = MongoObject(entries.toMap)

    val empty: MongoObject = MongoObject()
  }
}
