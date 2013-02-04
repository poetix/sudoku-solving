package com.codepoetics.sudoku

import Types._

case class SudokuAnalysis(freeCells: DigitGroupSet,
                          freeDigitsByRow: DigitGroupSet,
                          freeDigitsByCol: DigitGroupSet,
                          freeDigitsBySubGrid: DigitGroupSet) {
  def after(placedDigit: PlacedDigit): Option[SudokuAnalysis] = for {
    newFreeCells           <- removeDigit(freeCells, placedDigit.row, placedDigit.col)
    newFreeDigitsByRow     <- removeDigit(freeDigitsByRow, placedDigit.row, placedDigit.digit)
    newFreeDigitsByCol     <- removeDigit(freeDigitsByCol, placedDigit.col, placedDigit.digit)
    newFreeDigitsBySubGrid <- removeDigit(freeDigitsBySubGrid, placedDigit.subgrid, placedDigit.digit)
  } yield SudokuAnalysis(newFreeCells, newFreeDigitsByRow, newFreeDigitsByCol, newFreeDigitsBySubGrid)

  def removeDigit(data: DigitGroupSet, index: Int, digit: Int): Option[DigitGroupSet] = {
    val dataAtIndex = data(index - 1)
    if (!dataAtIndex.contains(digit)) None
    else Some(data.updated(index - 1, dataAtIndex -- digit))
  }

  val emptyCells = for {
    (freeCellsInRow, row) <- freeCells.zipWithIndex
    col <- freeCellsInRow.digits
  } yield (row, col - 1)

  val playableDigits = for {
    (row, col) <- emptyCells
    subgrid = ((col) /3) + ((row / 3) * 3)
  } yield (row, col, (freeDigitsByRow(row) and freeDigitsByCol(col) and freeDigitsBySubGrid(subgrid)))

  val isCompletable: Boolean = playableDigits.find(_._3.isEmpty).isEmpty
  lazy val afterForced: Option[SudokuAnalysis] = forcedMoves.foldLeft(Some(this):Option[SudokuAnalysis])( (state, move) => state.flatMap(_.after(move)))
  lazy val isValid = afterForced.isDefined

  lazy val forcedMoves: List[PlacedDigit] = for {
    (row, col, digitSet) <- playableDigits.filter(_._3.size == 1)
    digit                <- digitSet.digits
  } yield PlacedDigit(col + 1, row + 1, digit)

  lazy val possibleMoves: List[PlacedDigit] = {
    val (row, col, digitSet) = playableDigits.sortBy(d => (d._3.size, d._1, d._2)).head
    digitSet.digits.map { digit =>
      PlacedDigit(col + 1, row + 1, digit)
    }
  }
}

object SudokuAnalysis {
  def apply(): SudokuAnalysis = SudokuAnalysis(fullDigitGroupSet, fullDigitGroupSet, fullDigitGroupSet, fullDigitGroupSet)
}