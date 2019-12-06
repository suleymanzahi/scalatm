package bank

import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import bank.time.Date

import scala.collection.mutable.ArrayBuffer

/**
  * Creates a new bank with no accounts and no history.
  */

class Bank() {

  private var accounts: ArrayBuffer[BankAccount] = ArrayBuffer[BankAccount]()
  var nextAccountNumber = 999

  /**
    * Writes string to a file
    *
    * @param fileName Path of file
    * @param data     string written to fileName
    * @return
    */

  def save(fileName: String, data: String): Path =
    Files.write(Paths.get(fileName), data.getBytes("UTF-8"), StandardOpenOption.APPEND)

  /**
    * Returns a list of every bank account in the bank.
    * The returned list is sorted in alphabetical order based
    * on customer name.
    */
  def allAccounts(): Vector[BankAccount] = accounts.sortWith(_.holder.name < _.holder.name).toVector

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
        var s = ""
        val findAccount = findByNumber(account)

        findAccount match {
          case a: Some[BankAccount] => a.get.deposit(amount);
            s =
              s"""Transaktionen lyckades.
                 |${Date.now().toNaturalFormat}
                 |""".stripMargin
            save("C:\\Users\\suley\\Desktop\\Bank\\bank_log.txt", "\n" + {
              HistoryEntry(Date.now(), event).toLogFormat
            })
          case None => s = "Transaktionen misslyckades. Inget sådant konto hittades."
        }

        s

      }
      case Withdraw(account, amount) => { // use BankAccount.withdraw method, amount as param
        var s = ""
        val findAccount = findByNumber(account)

        findAccount match {
          case a: Some[BankAccount] => {
            if (a.get.withdraw(amount)) s =
              s"""Transaktionen lyckades.
                 |${Date.now().toNaturalFormat}
                 |""".stripMargin
            else s = "Transaktionen misslyckades. Otillräckligt saldo."
          }
          case None => s = "Transaktionen misslyckades. Inget sådant konto hittades."
        }
        s

      }
      case Transfer(accFrom, accTo, amount) => { // should use .getOrElse, if get return None -> might crash
        var s = ""
        val from = findByNumber(accFrom)
        val to = findByNumber(accTo)

        def validAccounts(): Boolean = from.isInstanceOf[Some[BankAccount]] && to.isInstanceOf[Some[BankAccount]]

        if (validAccounts() && from.get.withdraw(amount)) {
          to.get.deposit(amount)
          s =
            s"""Transaktionen lyckades.
               |${Date.now().toNaturalFormat}
               |""".stripMargin
        }
        else s = "Transaktionen misslyckades. Inget sådant konto hittades."
        s
      }
      case NewAccount(name, id) => {
        nextAccountNumber += 1
        val c = Customer(id, name)
        val ba = new BankAccount(c)
        ba.accountNumber = nextAccountNumber
        accounts += ba
        val s =
          s"""Nytt konto skapat med kontonummer:
             |${ba.accountNumber}
             |${Date.now().toNaturalFormat}
             |""".stripMargin
        s
      }
      case DeleteAccount(account) => { // remove from accounts sequence with -=
        var s = ""
        val deletion = findByNumber(account)

        deletion match {
          case a: Some[BankAccount] => accounts -= a.get; s = "Transaktionen lyckades."
          case None => s = "Transaktionen misslyckades. Inget sådant konto hittades."
        }
        s
      }
    }
  }

  /**
    * Returns a log of all changes to the bank's state.
    */
  def history(): Vector[HistoryEntry] = ???

  /**
    * Resets the bank to the state it had at the provided date.
    * Returns a string describing whether the event was
    * successful or failed.
    */
  def returnToState(returnDate: Date): String = ??? // delete all lines after given date(returnDate) ?

  // nextAccountNumber = 999 or minimumAccountNumber = accounts.map(ba => ba.accountNumber).min and set it
  // nextAccountNumber to this

}
