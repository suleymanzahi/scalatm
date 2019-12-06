package bank

import scala.io.StdIn._

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

  def deleteAccount(): Unit = {
    val account = readLine("Ange konto att radera:").toInt
    val deletion = DeleteAccount(account)
    val printAction = bank.doEvent(deletion)
    println(printAction)
  }

  def createAccount(): Unit = {
    val fname = readLine("Förnamn:")
    val lname = readLine("Efternamn:")
    val id = readLine("Id:").toLong
    val newAcc = NewAccount(id, {
      fname + " " + lname
    })
    val printAction = bank.doEvent(newAcc)
    println(printAction)
  }

  def depositAmount(): Unit = {
    val account = readLine("Konto:").toInt
    val amount = BigInt(readLine("Summa:"))
    val deposit = Deposit(account, amount)
    val printAction = bank.doEvent(deposit)
    println(printAction)
  }

  def withdrawAmount(): Unit = {
    val account = readLine("Konto:").toInt
    val amount = BigInt(readLine("Summa:"))
    val withdraw = Withdraw(account, amount)
    val printAction = bank.doEvent(withdraw)
    println(printAction)
  }

  def transferAmount(): Unit = {
    val accFrom = readLine("Konto att överföra från").toInt
    val accTo = readLine("Konto att överföra till").toInt
    val amount = BigInt(readLine("Summa:"))
    val transfer = Transfer(accFrom, accTo, amount)
    val printAction = bank.doEvent(transfer)
    println(printAction)

  }

  def main(args: Array[String]): Unit = {

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
          case 9 => ???
          case 10 => ???
          case 11 => loop = true; println("Avslutar...")
        }
      } catch {
        case e: Exception => (); println("Felaktig inmatning. Försök igen")
      }


    }


  }

}
