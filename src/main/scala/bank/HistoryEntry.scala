package bank

import bank.time.Date

/**
  * Represents the type and time of an event in the application
  * @param date When the event happened, created using the [[Date]] claass
  * @param event Type of event that happened
  */
case class HistoryEntry(date: Date, event: BankEvent) {
  /**
   * Returns a string suitable for writing to log files.
   */
  def toLogFormat: String = s"${date.toLogFormat} ${event.toLogFormat}"

  /**
   * Returns a string suitable for showing to users.
   */
  def toNaturalFormat: String = s"${date.toNaturalFormat}: ${event.toNaturalFormat}"
}

/**
  * Companion object for [[HistoryEntry]]
  */
object HistoryEntry {
  /**
   * Converts a string obtained from toLogFormat into a HistoryEntry object.
   */
  def fromLogFormat(str: String): HistoryEntry = {
    val xs = str.split(' ')
    val (date, event) = xs.splitAt(5)
    HistoryEntry(Date.fromLogFormat(date.mkString(" ")),
                 BankEvent.fromLogFormat(event.mkString(" ")))
  }
}
