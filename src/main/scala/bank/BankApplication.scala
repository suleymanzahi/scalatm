/**
  * Provides classes for creating and running a bank application
  */
package bank

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import bank.time.Date
import scala.io.StdIn._

/** Creates a bank application and runs it  */
object BankApplication {

  val menu: String =
    """
      |1. Hitta konton för en given kund
      |2. Sök efter kunder på (del av) namn
      |3. Sätt in pengar
      |4. Ta ut pengar
      |5. Överför pengar mellan konton
      |6. Skapa nytt konto
      |7. Radera existerande konto
      |8. Skriv ut alla konton i banken
      |9. Skriv ut ändringshistoriken
      |10. Återställ banken till ett tidigare datum
      |11. Avsluta
      |""".stripMargin

  val bank = new Bank

  /**
    * Recreates the bank from its previous state, prior to closing the application
    * @param fileName Path of the log file as string
    */
  def buildFromLogs(fileName: String): Unit = {
    val file = io.Source.fromFile(fileName).getLines.toVector
    bank.nextAccountNumber = 999
    for (i <- file.indices) {
      val eventString = file(i).split(' ').drop(5).mkString(" ")
      val builtEvent = BankEvent.fromLogFormat(eventString)
      val builtHistoryEntry = HistoryEntry.fromLogFormat(file(i))
      bank.doEvent(builtEvent)
      bank.historyEntries += builtHistoryEntry
    }
  }

  /**
    * Helper method to write to a file
    * @param fileName Path of the log file as string
    * @param data The content to be written to the log file
    * @return A Path instance of the log file
    */
  def writeToLog(fileName: String, data: String): Path =
    Files.write(Paths.get(fileName), (data + System.lineSeparator()).getBytes("UTF-8"), StandardOpenOption.APPEND)

  /**
    * Writes content to the log file and enters the eventtype in [[Bank.historyEntries]]
    * @param eventtype A instance of type [[BankEvent]]
    */
  def writeToLogAndHistory(eventtype: BankEvent): Unit = {
    if (eventtype.eventSuccess) {
      val entry = HistoryEntry(Date.now(), eventtype)
      bank.historyEntries += entry
      writeToLog("C:\\Users\\suley\\Desktop\\Bank\\test.txt",
        entry.toLogFormat)
    }
  }

  /**
    * Deletes an account, by creating a [[DeleteAccount]] instance
    */
  def deleteAccount(): Unit = {
    val account = readLine("Ange konto att radera:").toInt
    val deletion = DeleteAccount(account)
    val printAction = bank.doEvent(deletion)
    writeToLogAndHistory(deletion)
    println(printAction)
  }

  /**
  * Creates a new account, by creating a [[NewAccount]] instance
  */
  def createAccount(): Unit = {
    val fname = readLine("Förnamn:")
    val lname = readLine("Efternamn:")
    val id = readLine("Id:").toLong
    val newAcc = NewAccount(id, {
      fname + " " + lname
    })
    val printAction = bank.doEvent(newAcc)
    writeToLogAndHistory(newAcc)
    println(printAction)
  }

  /**
    * Deposits an amount to an account, by creating a [[Deposit]] instance
    */
  def depositAmount(): Unit = {
    val account = readLine("Konto:").toInt
    val amount = BigInt(readLine("Summa:"))
    val deposit = Deposit(account, amount)
    val printAction = bank.doEvent(deposit)
    writeToLogAndHistory(deposit)
    println(printAction)
  }

  /**
    * Withdraws an amount from an account, by creating a [[Withdraw]] instance
    */
  def withdrawAmount(): Unit = {
    val account = readLine("Konto:").toInt
    val amount = BigInt(readLine("Summa:"))
    val withdraw = Withdraw(account, amount)
    val printAction = bank.doEvent(withdraw)
    writeToLogAndHistory(withdraw)
    println(printAction)
  }

  /**
    * Transfers an amount between two accounts, by creating a [[Transfer]] instance
    */
  def transferAmount(): Unit = {
    val accFrom = readLine("Kontonummer att överföra från:").toInt
    val accTo = readLine("Kontonummer att överföra till:").toInt
    val amount = BigInt(readLine("Summa:"))
    val transfer = Transfer(accFrom, accTo, amount)
    val printAction = bank.doEvent(transfer)
    writeToLogAndHistory(transfer)
    println(printAction)

  }

  /**
    * Resets the bank to a certain date, by creating a [[Bank.returnToState]] instance
    */
  def resetDate(fileName: String): Unit = {
    println("Vilket datum vill du återställa banken till?")
    val year = readLine("År:").toInt
    val month = readLine("Månad:").toInt
    val day = readLine("Datum(dag):").toInt
    val hour = readLine("Timme:").toInt
    val minute = readLine("Minut:").toInt
    val newDate = Date(year, month, day, hour, minute)
    println(bank.returnToState(newDate, fileName))
  }

  def main(args: Array[String]): Unit = {

    buildFromLogs("C:\\Users\\suley\\Desktop\\Bank\\test.txt")

    var loop = false
    while (!loop) {

      println(menu)

      try {
      val readChoice = readLine("Val:").toInt

      readChoice match {
        case 1 => println(bank.findAccountsForHolder(readLine("Id:").toLong).mkString("")) //some sort of error handling should be added
        case 2 => println(bank.findByName(readLine("Namn:")).mkString("\n")) // same as above
        case 3 => depositAmount()
        case 4 => withdrawAmount()
        case 5 => transferAmount()
        case 6 => createAccount()
        case 7 => deleteAccount()
        case 8 => println(bank.allAccounts().mkString(""))
        case 9 => println(bank.history().map(he => he.toNaturalFormat).mkString("\n"))
        case 10 => resetDate("C:\\Users\\suley\\Desktop\\Bank\\test.txt")
        case 11 => loop = true; println("Avslutar...")
        case _ => println("Felaktig inmatning. Försök igen")
      }

       } catch {
        case e: Exception => (); println("Felaktig inmatning. Försök igen")
      }


    }


  }

}
