package bank

/**
  * Creates a new bank account for the customer provided.
  * The account is given a unique account number and initially
  * has a balance of 0 kr.
  */
class BankAccount(val holder: Customer) {

  private var _balance: BigInt = 0

  val accountNumber: Int = ???

  /**
    * Deposits the provided amount in this account.
    */
  def deposit(amount: BigInt): Unit = _balance += amount

  /**
    * Returns the balance of this account.
    */
  def balance: BigInt = _balance

  /**
    * Withdraws the provided amount from this account,
    * if there is enough money in the account. Returns true
    * if the transaction was successful, otherwise false.
    */
  def withdraw(amount: BigInt): Boolean = {
    if (amount <= _balance) {
      _balance -= amount
      true
    } else false
  }


  override def toString(): String = ???

}

