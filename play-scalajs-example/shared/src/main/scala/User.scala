package model

case class User(
  name: String,
  age: Int,
  email: Option[String],
  isAlive: Boolean
)
