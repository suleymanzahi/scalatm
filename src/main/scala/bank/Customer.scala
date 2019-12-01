package bank

/**
  * Describes a customer of a bank with provided name and id.
  */

case class Customer(name: String, id: Long) {
  override def toString(): String = s"$name, $id"
}
