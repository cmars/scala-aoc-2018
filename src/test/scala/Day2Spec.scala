import org.specs2.Specification

class Day2Spec extends Specification { def is = s2"""
  Test Day 2 $ok
     classify_none    $classify_none
     classify_2only   $classify_2only
     classify_3only   $classify_3only
     classify_2and3   $classify_2and3
  """

  def classify_none = Day2.classifyID("bar") must_== (false,false)
  def classify_2only = (
    (Day2.classifyID("foo") must_== (true,false))
    and (Day2.classifyID("oof") must_== (true, false))
    and (Day2.classifyID("food") must_== (true, false)))
  def classify_3only = (
    (Day2.classifyID("wooo") must_== (false,true))
    and (Day2.classifyID("ooow") must_== (false,true))
    and (Day2.classifyID("wooop") must_== (false,true)))
  def classify_2and3 = Day2.classifyID("wooohaa") must_== (true,true)
}
