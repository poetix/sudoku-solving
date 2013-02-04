package com.codepoetics.sudoku

import Stream.cons

import Types._

case class DigitGroup(bits: Int) {

  val isEmpty = bits == 0
  val size = (1 to 9).foldLeft(0)( (total, digit) => if (contains(digit)) total + 1 else total)
  def contains(digit: Int): Boolean = (bits & (1 << digit >> 1)) != 0

  def ++(digit: Int): DigitGroup = DigitGroup(bits | (1 << digit >> 1))
  def ++(other: DigitGroup): DigitGroup = DigitGroup(bits | other.bits)
  def --(digit: Int): DigitGroup = DigitGroup(bits & ~(1 << digit >> 1))
  def --(other: DigitGroup): DigitGroup = DigitGroup(bits & ~(other.bits))
  def and(other: DigitGroup): DigitGroup = DigitGroup(bits & other.bits)

  def inverted(): DigitGroup = DigitGroup((1 << 9) - 1 - bits)

  def digits(): List[Int] = (1 to 9).filter(contains(_)).toList

  override def toString(): String = ((1 to 9) map { digit =>
    if (contains(digit)) digit.toString else "x"
  }).mkString("Digit Group: [", " ", "]")
}

object DigitGroup {
  def apply(digits: Int*): DigitGroup = digits.foldLeft(DigitGroup(0))( _ ++ _ )
}