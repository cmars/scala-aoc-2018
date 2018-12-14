import scala.io.Source

object Day5 {
  val terminator = List(None)

  def main(args: Array[String]): Unit = {
    var src: List[Char] = Source.fromFile("day/5/input").toList
    if (!src(src.length - 1).isLetter)  // newlines, sheesh :P
      src = src.slice(0, src.length - 1)
    src = react(src)
    println(src.length)

    val types = "abcdefghijklmnopqrstuvwxyz".toList
    types.map({ t: Char => (t, react(withoutType(src, t)).length) }).foldLeft(None: Option[(Char, Int)]) {
      (acc, x) => {
        val (currentT, currentLen) = x
        acc match {
          case None => Some(x)
          case Some((_: Char, minLen: Int)) => {
            if (currentLen < minLen) {
              Some((currentT, currentLen))
            } else {
              acc
            }
          }
        }
      }
    } match {
      case None => println("could not determine best type to remove")
      case Some((t: Char, len: Int)) => println("best type to remove: " + t + " at length " + len)
    }
  }

  def withoutType(src: List[Char], t: Char): List[Char] = {
    src.filter(ch => ch.toLower != t.toLower)
  }

  def react(src: List[Char]): List[Char] = {
    var s = src
    var reactive = true
    while (reactive && s.nonEmpty) {
      reactive = false
      var skipNext = false
      s = s.zip(s.drop(1).map(ch => Some(ch)) ++ terminator.toIterator).filter(pair => {
        if (skipNext) {
          skipNext = false
          false
        } else {
          pair match {
            case (a: Char, Some(b: Char)) =>
              if ((a.isLower == b.isUpper) && (a.toLower == b.toLower)) {
                reactive = true
                skipNext = true
                false
              } else {
                true
              }
            case _ => true
          }
        }
      }).map(pair => pair._1)
    }
    s
  }
}

// This one probably incurs a lot more heap allocations.
object Day5BitByBit {
  val terminator = List(None)

  def main(args: Array[String]): Unit = {
    var polymer: List[Char] = Source.fromFile("day/5/input").toList
    if (!polymer(polymer.length - 1).isLetter)
      polymer = polymer.slice(0, polymer.length - 1)
    polymer = react(polymer)
    println(polymer.length)
    println(polymer.mkString)
  }

  def react(start: List[Char]): List[Char] = {
    var s = start
    var i: Int = 1
    while (i < s.length) {
      val (a, b) = (s(i - 1), s(i))
      if ((a.isLower == b.isUpper) && (a.toLower == b.toLower)) {
        val right = if (i < s.length) {
          s.slice(i + 1, s.length)
        } else {
          List()
        }
        s = s.slice(0, i - 1) ++ right
        i = math.max(i - 2, 1)
      } else {
        i += 1
      }
    }
    s
  }
}