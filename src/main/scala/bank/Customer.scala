package bank

/**
  * Describes a customer of a bank with provided name and id.
  * @param name Name to be given to customer
  * @param id ID to be given to Customer
  */

case class Customer(name: String, id: Long) {
  override def toString(): String = s"$name, id $id"
}
