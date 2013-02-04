package com.codepoetics.sudoku

import Types._

case class SudokuState(placedDigits: List[PlacedDigit], analysis: SudokuAnalysis) {
  def isComplete = placedDigits.size == 81
  def isEmpty = placedDigits.isEmpty
  def after(placedDigit: PlacedDigit) = SudokuState(placedDigit :: placedDigits, analysis.after(placedDigit))
  def afterAll(digits: Iterable[PlacedDigit]) = digits.foldLeft(SudokuState())( (state, digit) => state.after(digit) )
}

object SudokuState {
  def apply(): SudokuState = new SudokuState(Nil, SudokuAnalysis())
}