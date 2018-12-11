import java.text.SimpleDateFormat
import java.time.LocalDateTime
import java.util.Date

import scala.io.Source

object Day4 {
  val timestampFormat = new SimpleDateFormat("[yyyy-MM-dd HH:mm]")

  def main(args: Array[String]): Unit = {
    Source.fromFile("day/4/input").getLines().toStream.sorted.map(line => {
      parseEvent(line)
    }).foreach(println)
  }

  def parseEvent(s: String): Event = {
    val parts = s.split(' ')
    val timestamp = timestampFormat.parse(parts(0) + " " + parts(1))
    if (parts(2) == "Guard" && parts(4) == "begins") {
      BeginShift(timestamp, parts(3))
    } else if ((parts(2), parts(3)) == ("falls", "asleep")) {
      FellAsleep(timestamp)
    } else if ((parts(2), parts(3)) == ("wakes", "up")) {
      WokeUp(timestamp)
    } else {
      null
    }
  }
}

abstract class Event(timestamp: Date)

case class BeginShift(timestamp: Date, guardID: String) extends Event(timestamp)

case class FellAsleep(timestamp: Date) extends Event(timestamp)

case class WokeUp(timestamp: Date) extends Event(timestamp)
