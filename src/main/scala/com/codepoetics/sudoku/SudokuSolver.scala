package com.codepoetics.sudoku

import com.codepoetics.sudoku.Types.PlacedDigit

object SudokuSolver {

  var previousStates = Set.empty[Set[PlacedDigit]]
  def solve(state: SudokuState): Option[SudokuState] = {
    val placedDigits = state.placedDigits.toSet
    if (previousStates.contains(placedDigits)) throw new Exception("We have been here before")
    previousStates  += placedDigits.toSet

    println(previousStates.size + "\n")

    if (!state.isValid || !state.isCompletable) None
    else if (state.isComplete) Some(state)
    else if (state.hasForcedMoves) state.afterForced.flatMap { newState => solve(newState) }
    else {
      println(state)
      state.analysis.possibleMoves.flatMap { possibleMove =>
        state.after(possibleMove).flatMap( newState => solve(newState)).toStream
      }.headOption
    }
  }
}
