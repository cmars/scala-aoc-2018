import org.specs2.Specification

class Day5Spec extends Specification { def is = s2"""
  Test Day 5 $ok
     AaB   $AaB
     Aa    $Aa
     abc   $abc
     aaa   $aaa
     aa    $aa
     ab    $ab
     ABba  $ABba
     abAB  $abAB
     GABbag  $GABbag
     example $example
     another $another
     another2 $another2
  """

  def AaB = Day5.react("AaB".toList).mkString must_== "B"
  def Aa = Day5.react("Aa".toList).mkString must_== ""
  def abc = Day5.react("abc".toList).mkString must_== "abc"
  def aaa = Day5.react("aaa".toList).mkString must_== "aaa"
  def aa = Day5.react("aa".toList).mkString must_== "aa"
  def ab = Day5.react("ab".toList).mkString must_== "ab"
  def ABba = Day5.react("ABba".toList).mkString must_== ""
  def abAB = Day5.react("abAB".toList).mkString must_== "abAB"
  def GABbag = Day5.react("GABbag".toList).mkString must_== ""
  def example = Day5.react("dabAcCaCBAcCcaDA".toList).mkString must_== "dabCBAcaDA"
  def another = Day5.react("fooGgBb".toList).mkString must_== "foo"
  def another2 = Day5.react("foGgOdBbOoD".toList).mkString must_== "f"

}
