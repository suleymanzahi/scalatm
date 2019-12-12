package bank

import java.nio.file.{Files, Paths}
import bank.time.Date
import scala.collection.mutable.ArrayBuffer
import io.Source._

/**
  * Creates a new bank with no accounts and no history.
  */
class Bank() {

  private var accounts: ArrayBuffer[BankAccount] = ArrayBuffer[BankAccount]()
  /** A list of all event and their respective date, created as instances of [[HistoryEntry]] */
  var historyEntries: ArrayBuffer[HistoryEntry] = ArrayBuffer[HistoryEntry]()
  /** Used to generate the account number for the next account to be created */
  var nextAccountNumber = 999


  /**
    * Returns a list of every bank account in the bank, in alphabetical order
    */
  def allAccounts(): Vector[BankAccount] =
    accounts.sortWith(_.holder.name < _.holder.name).toVector


  /**
    * Returns the account holding the provided account number
    * @param accountNbr Unique number of the bank account
    */
  def findByNumber(accountNbr: Int): Option[BankAccount] =
    accounts.find(c => c.accountNumber == accountNbr)

  /**
    * Returns a list of every account belonging to the customer
    * @param id The ID of the customer
    */
  def findAccountsForHolder(id: Long): Vector[BankAccount] =
    accounts.filter(x => x.holder.id == id).toVector

  /**
    * Returns a list of all customers whose names match the provided name pattern.
    */
  def findByName(namePattern: String): Vector[Customer] =
    accounts.map(ba => ba.holder).filter(c => c.name.toLowerCase.contains(namePattern.toLowerCase)).toVector


  /**
    * Helper method for returning a string describing an event in [[doEvent]]
    * @param description Text to be formatted
    * @return Returns a string formatted with the current date
    */
  def returnEventDescription(description: String): String = {
    s"""$description
       |${Date.now().toNaturalFormat}
       |""".stripMargin
  }

  /**
    * Executes an event in the bank.
    * @return Returns a string describing whether the event was successful or failed.
    * For example, if a new account is to be created, a [[NewAccount]] is taken as argument.
    * [[doEvent]] instantiates a new [[BankAccount]] and assigns [[NewAccount]]'s arguments to
    * the [[BankAccount]] instance. Same procedures are used to execute events in [[doEvent]]
    */

  def doEvent(event: BankEvent): String = {
    event match {
      case Deposit(account, amount) => { // use BankAccount.deposit method, amount as param
        val findAccount = findByNumber(account)

        findAccount match {
          case a: Some[BankAccount] => a.get.deposit(amount)
            event.eventSuccess = true
            returnEventDescription("Transaktionen lyckades")

          case None => returnEventDescription("Transaktionen misslyckades. Inget sådant konto hittades.")
        }

      }
      case Withdraw(account, amount) => { // use BankAccount.withdraw method, amount as param

        val findAccount = findByNumber(account)

        findAccount match {
          case a: Some[BankAccount] => {
            if (a.get.withdraw(amount)) {
              event.eventSuccess = true
              returnEventDescription("Transaktionen lyckades")
            }
            else returnEventDescription("Transaktionen misslyckades. Otillräckligt saldo.")
          }
          case None => returnEventDescription("Transaktionen misslyckades. Inget sådant konto hittades.")
        }


      }
      case Transfer(accFrom, accTo, amount) => {

        val from = findByNumber(accFrom)
        val to = findByNumber(accTo)

        def validAccounts(): Boolean = from.isInstanceOf[Some[BankAccount]] && to.isInstanceOf[Some[BankAccount]]

        if (validAccounts() && from.get.withdraw(amount)) {
          event.eventSuccess = true
          to.get.deposit(amount)
          returnEventDescription("Transaktionen lyckades")

        }
        else returnEventDescription("Transaktionen misslyckades. Fel konto angivet eller summan överstiger saldot.")
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
        event.eventSuccess = true
        newAccountDescription
      }
      case DeleteAccount(account) => { // remove from accounts sequence with -=

        val deletion = findByNumber(account)

        deletion match {
          case a: Some[BankAccount] => {
            event.eventSuccess = true
            accounts -= a.get
            returnEventDescription("Transaktionen lyckades")
          }
          case None => returnEventDescription("Transaktionen misslyckades. Inget sådant konto hittades.")
        }

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
  def returnToState(returnDate: Date, fileName: String): String = {
    import BankApplication._ // error handling for if date is wrong should be added

    val currentFile = fromFile(fileName).getLines.toVector
    def index = history().indexWhere(he => returnDate.compare(he.date) < 0)
    if (index != -1 && index != 0) { // because indexWhere returns -1 if not found index.
      val newFile = currentFile.take(index)
      historyEntries.clear()
      accounts.clear()
      Files.write(Paths.get(fileName), (newFile.mkString("\n") + System.lineSeparator()).getBytes("UTF-8"))
      buildFromLogs(fileName)
      returnEventDescription("Banken återställd.")

    }
    else returnEventDescription("Banken kunde inte återställas. Datum ej funnet.")
    }
  }



