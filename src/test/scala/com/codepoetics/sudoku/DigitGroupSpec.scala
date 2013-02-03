package com.codepoetics.sudoku

import org.specs2.Specification
import org.specs2.matcher.Matcher

case class DigitGroup(val bits: Int) {
  def contains(digit: Int): Boolean = (bits & (1 << digit)) != 0

  def ++(digit: Int): DigitGroup = DigitGroup(bits | (1 << digit))
  def ++(other: DigitGroup): DigitGroup = DigitGroup(bits | other.bits)
  def --(digit: Int): DigitGroup = DigitGroup(bits & ~(1 << digit))
  def --(other: DigitGroup): DigitGroup = DigitGroup(bits & ~(other.bits))

  override def toString(): String = ((1 to 9) map { digit =>
    if (contains(digit)) digit.toString else "x"
  }).mkString("Digit Group: [", " ", "]")
}

object DigitGroup {
  def apply(digits: Int*): DigitGroup = DigitGroup(digits.foldLeft(0)((acc, digit) => acc | (1 << digit)))
}

class DigitGroupSpec extends Specification {

  def containOnlyTheDigits(digits: Int*): Matcher[DigitGroup] = ({ group: DigitGroup =>
    (1 to 9).foldLeft(true)((result, digit) =>
      if (digits.contains(digit)) group.contains(digit) && result
      else !group.contains(digit) && result
    )
  }, "does not contain only the digits %s".format(digits.mkString("[", ", ", "]")))

  def is =
  "A DigitGroup" ^
    "should record which digits are present and which are absent within the group" ! {
      val group = DigitGroup(1, 5, 7, 9)
      group must containOnlyTheDigits(1, 5, 7, 9)
    } ^
    "should be able to make a copy of itself with a digit added" ! {
      val group = DigitGroup(1, 5, 7, 9)
      (group ++ 3) must containOnlyTheDigits(1, 3, 5, 7, 9)
    } ^
    "should be able to make a copy of itself with a digit removed" ! {
      val group = DigitGroup(1, 5, 7, 9)
      (group -- 5) must containOnlyTheDigits(1, 7, 9)
    } ^
    "should be able to merge itself with another digit group" ! {
      DigitGroup(1, 5, 7, 9)
      (DigitGroup(1, 5, 7) ++ DigitGroup(7, 9)) must containOnlyTheDigits(1, 5, 7, 9)
    } ^
    "should be able to mask itself with another digit group" ! {
      DigitGroup(1, 5, 7, 9)
      (DigitGroup(1, 5, 7) -- DigitGroup(7, 9)) must containOnlyTheDigits(1, 5)
    } ^ end
}
