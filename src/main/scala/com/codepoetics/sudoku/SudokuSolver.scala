package com.codepoetics.sudoku

import com.codepoetics.sudoku.Types.PlacedDigit

object SudokuSolver {

  def solve(state: SudokuState): Option[SudokuState] = {
    if (!state.isValid || !state.isCompletable) None
    else if (state.isComplete) Some(state)
    else if (state.hasForcedMoves) state.afterForced.flatMap { newState => solve(newState) }
    else {
      state.analysis.possibleMoves.flatMap { possibleMove =>
        state.after(possibleMove).flatMap( newState => solve(newState)).toStream
      }.headOption
    }
  }
}
