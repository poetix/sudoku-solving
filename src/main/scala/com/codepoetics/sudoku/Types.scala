package com.codepoetics.sudoku

object Types {

  case class PlacedDigit(col: Int, row: Int, digit: Int) {
    val subgrid = ((col - 1) / 3) + (((row - 1) / 3) * 3) + 1
  }

  type DigitGroupSet = List[DigitGroup]

  val emptyDigitGroup = DigitGroup()
  val fullDigitGroup = emptyDigitGroup.inverted

  val emptyDigitGroupSet: DigitGroupSet = List(
    emptyDigitGroup,
    emptyDigitGroup,
    emptyDigitGroup,
    emptyDigitGroup,
    emptyDigitGroup,
    emptyDigitGroup,
    emptyDigitGroup,
    emptyDigitGroup,
    emptyDigitGroup)

  val fullDigitGroupSet: DigitGroupSet = emptyDigitGroupSet.map(_.inverted)
}