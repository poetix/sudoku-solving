package com.codepoetics.sudoku

import Types._

case class SudokuState(placedDigits: List[PlacedDigit], analysis: SudokuAnalysis) {
  def isCompletable = analysis.isCompletable
  def isValid = analysis.isValid
  def isComplete = placedDigits.size == 81
  def isEmpty = placedDigits.isEmpty
  def hasForcedMoves = analysis.forcedMoves.size > 0

  def after(placedDigit: PlacedDigit): Option[SudokuState] = analysis.after(placedDigit).map { newAnalysis =>
    SudokuState(placedDigit :: placedDigits, newAnalysis)
  }
  def afterForced: Option[SudokuState] = analysis.afterForced.map { newAnalysis =>
    new SudokuState(analysis.forcedMoves.foldLeft(placedDigits)((placedDigits, digit) => digit :: placedDigits), newAnalysis)
  }
  def afterAll(digits: Iterable[PlacedDigit]) = digits.foldLeft(Some(this):Option[SudokuState])( (state, digit) => state.flatMap(_.after(digit) ))

  override def toString():String = {
    ((1 to 9) map { row:Int =>
      ((1 to 9) map { col:Int =>
        placedDigits.find( p => p.row == row && p.col == col).map(_.digit.toString).getOrElse("x")
      }).mkString
    }).mkString("\n") + "\n"
  }
}

object SudokuState {
  def apply(): SudokuState = new SudokuState(Nil, SudokuAnalysis())
}