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


  def main(args: Array[String]): Unit = {


    var loop = false
    while (!false) {
      println(menu)
      val readChoice = readLine("Val:").toInt

      readChoice match {
        case 1 => println("hejsan")
        case 2 => println("Avslutar...") ; loop = true
      }
    }


  }

}
