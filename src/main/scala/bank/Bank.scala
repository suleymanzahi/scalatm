package bank

import bank.time.Date

import scala.collection.mutable.ArrayBuffer

/**
  * Creates a new bank with no accounts and no history.
  */

class Bank() {

  var accounts: ArrayBuffer[BankAccount] = ArrayBuffer[BankAccount]()
  var nextAccountNumber = 1000

  def addAccount(c: Customer) : BankAccount = { NewAccount
    nextAccountNumber += 1
    val newAcc = new BankAccount(c)
    newAcc.accountNumber = nextAccountNumber
    newAcc
  }

  /**
    * Returns a list of every bank account in the bank.
    * The returned list is sorted in alphabetical order based
    * on customer name.
    */
  def allAccounts(): Vector[BankAccount] = ??? // accounts.map(c => c.holder).sorted.toVector

  /*
  def load(fileName: String): Vector[String] =
   io.Source.fromFile(fileName).getLines.toVector maybe?
  * */
  /**
    * Returns the account holding the provided account number.
    */
  def findByNumber(accountNbr: Int): Option[BankAccount] = accounts.find(c => c.accountNumber == accountNbr)
  /**
    * Returns a list of every account belonging to
    * the customer with the provided id.
    */
  def findAccountsForHolder(id: Long): Vector[BankAccount] = accounts.filter(x => x.holder.id == id).toVector
  /**
    * Returns a list of all customers whose names match
    * the provided name pattern.
    */
  def findByName(namePattern: String): Vector[Customer] = accounts.map(bankacc => bankacc.holder).filter(cust => cust.name == namePattern).toVector

  /**
    * Executes an event in the bank.
    * Returns a string describing whether the
    * event was successful or failed.
    */
  def doEvent(event: BankEvent): String = ???
  /**
    * Returns a log of all changes to the bank's state.
    */
  def history(): Vector[HistoryEntry] = ???
  /**
    * Resets the bank to the state it had at the provided date.
    * Returns a string describing whether the event was
    * successful or failed.
    */
  def returnToState(returnDate: Date): String = ???

}
