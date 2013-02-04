package com.codepoetics.sudoku

import Types._

case class SudokuAnalysis(freeCells: DigitGroupSet,
                          freeDigitsByRow: DigitGroupSet,
                          freeDigitsByCol: DigitGroupSet,
                          freeDigitsBySubGrid: DigitGroupSet) {
  def after(placedDigit: PlacedDigit): SudokuAnalysis = SudokuAnalysis(
    removeDigit(freeCells, placedDigit.row, placedDigit.col),
    removeDigit(freeDigitsByRow, placedDigit.row, placedDigit.digit),
    removeDigit(freeDigitsByCol, placedDigit.col, placedDigit.digit),
    removeDigit(freeDigitsBySubGrid, placedDigit.subgrid, placedDigit.digit)
  )

  def removeDigit(data: DigitGroupSet, index: Int, digit: Int) = data.updated(index - 1, data(index - 1) -- digit)

  val emptyCells = for {
    (freeCellsInRow, row) <- freeCells.zipWithIndex
    col <- freeCellsInRow.digits
  } yield (row, col - 1)

  val playableDigits = for {
    (row, col) <- emptyCells
    subgrid = ((col) % 3) + ((row) / 3)
  } yield (row, col, (freeDigitsByRow(row) and freeDigitsByCol(col) and freeDigitsBySubGrid(subgrid)))

  val isCompletable: Boolean = playableDigits.find(_._3.isEmpty).isDefined

  val possibleMoves: Stream[PlacedDigit] = for {
    (row, col, digitSet) <- playableDigits.sortBy(_._3.size).toStream
    digit    <- digitSet.digits.toStream
  } yield PlacedDigit(row + 1, col + 1, digit)
}

object SudokuAnalysis {
  def apply(): SudokuAnalysis = SudokuAnalysis(fullDigitGroupSet, fullDigitGroupSet, fullDigitGroupSet, fullDigitGroupSet)
}