package bank

import bank.time.Date
import scala.collection.mutable.ArrayBuffer

/**
  * Creates a new bank with no accounts and no history.
  */

class Bank() {

  private var accounts: ArrayBuffer[BankAccount] = ArrayBuffer[BankAccount]()
  var historyEntries: ArrayBuffer[HistoryEntry] = ArrayBuffer[HistoryEntry]()
  var nextAccountNumber = 999


  /**
    * Returns a list of every bank account in the bank.
    * The returned list is sorted in alphabetical order based
    * on customer name.
    */
  def allAccounts(): Vector[BankAccount] =
    accounts.sortWith(_.holder.name < _.holder.name).toVector


  /**
    * Returns the account holding the provided account number.
    */
  def findByNumber(accountNbr: Int): Option[BankAccount] =
    accounts.find(c => c.accountNumber == accountNbr)

  /**
    * Returns a list of every account belonging to
    * the customer with the provided id.
    */
  def findAccountsForHolder(id: Long): Vector[BankAccount] =
    accounts.filter(x => x.holder.id == id).toVector

  /**
    * Returns a list of all customers whose names match
    * the provided name pattern.
    */
  def findByName(namePattern: String): Vector[Customer] =
    accounts.map(ba => ba.holder).filter(c => c.name.toLowerCase.contains(namePattern.toLowerCase)).toVector

  /**
    * Executes an event in the bank.
    * Returns a string describing whether the
    * event was successful or failed.
    */

  def doEvent(event: BankEvent): String = {
    event match {
      case Deposit(account, amount) => { // use BankAccount.deposit method, amount as param
        var depositDescription = ""
        val findAccount = findByNumber(account)

        findAccount match {
          case a: Some[BankAccount] => a.get.deposit(amount)
            depositDescription =
              s"""Transaktionen lyckades.
                 |${Date.now().toNaturalFormat}
                 |""".stripMargin
            event.bool = true
          case None => depositDescription = "Transaktionen misslyckades. Inget sådant konto hittades."
        }

        depositDescription

      }
      case Withdraw(account, amount) => { // use BankAccount.withdraw method, amount as param
        var withdrawDescription = ""
        val findAccount = findByNumber(account)

        findAccount match {
          case a: Some[BankAccount] => {
            event.bool = true
            if (a.get.withdraw(amount)) {
              withdrawDescription =
                s"""Transaktionen lyckades.
                   |${Date.now().toNaturalFormat}
                   |""".stripMargin
            }
            else withdrawDescription = "Transaktionen misslyckades. Otillräckligt saldo."
          }
          case None => withdrawDescription = "Transaktionen misslyckades. Inget sådant konto hittades."
        }
        withdrawDescription

      }
      case Transfer(accFrom, accTo, amount) => { // should use .getOrElse, if get return None -> might crash
        var transferDescription = ""
        val from = findByNumber(accFrom)
        val to = findByNumber(accTo)

        def validAccounts(): Boolean = from.isInstanceOf[Some[BankAccount]] && to.isInstanceOf[Some[BankAccount]]

        if (validAccounts() && from.get.withdraw(amount)) {
          event.bool = true
          to.get.deposit(amount)
          transferDescription =
            s"""Transaktionen lyckades.
               |${Date.now().toNaturalFormat}
               |""".stripMargin

        }
        else transferDescription = "Transaktionen misslyckades. Inget sådant konto hittades."
        transferDescription
      }
      case NewAccount(name, id) => {
        nextAccountNumber += 1
        val c = Customer(id, name)
        val ba = new BankAccount(c)

        ba.accountNumber = nextAccountNumber
        accounts += ba
        val newAccountDescription =
          s"""Nytt konto skapat med kontonummer:
             |${ba.accountNumber}
             |${Date.now().toNaturalFormat}
             |""".stripMargin
        event.bool = true
        newAccountDescription
      }
      case DeleteAccount(account) => { // remove from accounts sequence with -=
        var deletionDescription = ""
        val deletion = findByNumber(account)

        deletion match {
          case a: Some[BankAccount] => {
            event.bool = true
            accounts -= a.get
            deletionDescription = "Transaktionen lyckades."
          }
          case None => deletionDescription = "Transaktionen misslyckades. Inget sådant konto hittades."
        }
        deletionDescription
      }
    }
  }

  /**
    * Returns a log of all changes to the bank's state.
    */
  def history(): Vector[HistoryEntry] = historyEntries.toVector

  /**
    * Resets the bank to the state it had at the provided date.
    * Returns a string describing whether the event was
    * successful or failed.
    */
  def returnToState(returnDate: Date): String = ???

  //TODO:
  // delete all lines after given date(returnDate) ?
  // delete those instances as well ?
  // minimumAccountNumber = accounts.map(ba => ba.accountNumber).max and set
  // nextAccountNumber to this
  // pseudokod: radera rader från logfil ; återbygg med buildFromLogs

}
