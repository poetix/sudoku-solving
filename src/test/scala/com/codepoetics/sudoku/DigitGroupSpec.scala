package com.codepoetics.sudoku

import org.specs2.Specification
import org.specs2.matcher.Matcher

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
    } ^
    "should be able to list its contained digits" ! {
      DigitGroup(1, 5, 7, 9).digits must beEqualTo(List(1, 5, 7, 9))
    } ^
    "is invertable" ! {
      DigitGroup(1, 5, 7, 9).inverted must beEqualTo(DigitGroup(2, 3, 4, 6, 8))
    } ^end
}
