package com.codepoetics.sudoku

object SudokuSolver {
  def solve(state: SudokuState): Option[SudokuState] = {
    if (state.isComplete) Some(state)
      else if (!state.analysis.isCompletable) None
      else state.analysis.possibleMoves.flatMap { possibleMove =>
        println(possibleMove)
        solve(state.after(possibleMove)).toStream
      }.headOption
  }
}
