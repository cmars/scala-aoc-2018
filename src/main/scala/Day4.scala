import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import scala.collection.mutable
import scala.io.Source

object Day4 {
  val timestampFormat = new SimpleDateFormat("[yyyy-MM-dd HH:mm]")

  def main(args: Array[String]): Unit = {
    val processor = new Processor
    Source.fromFile("day/4/input").getLines().toStream.sorted.foreach(line => {
      val ev = parseEvent(line)
      processor.processEvent(ev)
    })
    processor.sleepiest match {
      case Some(g) => println(g.ID, g.sleepMinutes, g.sleepiestMinute)
      case None => println("no guard found!")
    }
    val sleepiestGuardMinute = processor.guards.values.foldLeft((None: Option[Guard], None: Option[(Int, Int)])) {
      (acc, guard) => {
        guard.sleepiestMinute match {
          case None => acc
          case Some((minute: Int, hits: Int)) => {
            acc match {
              case (None, _) => (Some(guard), guard.sleepiestMinute)
              case (Some(_: Guard), Some((_: Int, leaderHits: Int))) => {
                if (hits > leaderHits) {
                  (Some(guard), Some((minute, hits)))
                } else {
                  acc
                }
              }
              case _ => acc
            }
          }
        }
      }
    }
    sleepiestGuardMinute match {
      case (Some(guard: Guard), Some((minute: Int, hits: Int))) => {
        println("sleepiest guard minute", guard.ID, minute, hits)
      }
      case _ => println("could not determine sleepiest guard-minute")
    }
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

class Guard(id: String) {
  val ID = id
  var sleepMinutes: Long = 0
  var minutes: mutable.HashMap[Int, Int] = new mutable.HashMap
  var sleepiestMinute: Option[(Int, Int)] = None
}

class Processor {
  val guards: mutable.HashMap[String, Guard] = new mutable.HashMap
  var currentGuard: Option[Guard] = None
  var currentEvent: Option[Event] = None
  var sleepiest: Option[Guard] = None

  def processEvent(ev: Event): Unit = {
    (currentGuard, currentEvent, ev) match {
      case (_, _, BeginShift(ts, id)) => {
        val guard = guards.getOrElseUpdate(id, new Guard(id))
        currentGuard = Some(guard)
        currentEvent = Some(WokeUp(ts))  // assuming guards start their shift awake :)
      }
      case (Some(guard: Guard), Some(FellAsleep(sleepFrom)), WokeUp(sleepTo)) => {
        guard.sleepMinutes += sleepTo.getTime() - sleepFrom.getTime()
        var t = sleepFrom.getTime()
        while (t < sleepTo.getTime()) {
          val minute = new Date(t).getMinutes()
          var hits = guard.minutes.getOrElse(minute, 0)
          hits += 1
          guard.minutes.put(minute, hits)

          guard.sleepiestMinute match {
            case Some((_, maxHits: Int)) => {
              if (hits > maxHits) {
                guard.sleepiestMinute = Some((minute, hits))
              }
            }
            case None => guard.sleepiestMinute = Some((minute, hits))
          }

          t += 60000
          guard.sleepMinutes += 1
        }

        sleepiest match {
          case Some(leader) => {
            if (guard.sleepMinutes > leader.sleepMinutes) {
              sleepiest = Some(guard)
            }
          }
          case None => sleepiest = Some(guard)
        }
      }
      case _ =>
    }
    currentEvent = Some(ev)
  }
}