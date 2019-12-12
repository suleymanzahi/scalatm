package bank

/**
  * Creates a new bank account for the customer provided.
  * @param holder A customer to be assigned to the bank account
  */
class BankAccount(val holder: Customer) {

  /** The account number */
  var accountNumber: Int = 0

  private var _balance: BigInt = 0

  /**
    * Deposits the provided amount in this account.
    */
  def deposit(amount: BigInt): Unit = {
    require(amount >= 0, "Summan kan ej vara mindre än noll.")
    _balance += amount
  }

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
    if (amount <= _balance && !(amount < 0)) {
      _balance -= amount
      true
    } else false
  }

  override def toString(): String =
    s"""
       |Konto $accountNumber (${holder.name}, id ${holder.id})
       |$balance kr
       |""".stripMargin //behöver mer detaljer som id, saldo etc, bestämmer utskrift av konton

}






