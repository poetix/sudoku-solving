package com.codepoetics.sudoku

import org.specs2.Specification
import Types._

class SudokuSolverSpec extends Specification {
  def is =
  "The SudokuSolver" ^
    "should solve an easy sudoku puzzle" ! {
      val state = SudokuState().afterAll(Seq(
        PlacedDigit(1, 1, 3),
        PlacedDigit(2, 2, 6),
        PlacedDigit(1, 3, 1),
        PlacedDigit(3, 3, 9),

        PlacedDigit(6, 1, 5),
        PlacedDigit(4, 2, 1),
        PlacedDigit(5, 2, 2),

        PlacedDigit(9, 1, 1),
        PlacedDigit(8, 2, 4),
        PlacedDigit(7, 3, 3),

        PlacedDigit(3, 4, 1),
        PlacedDigit(2, 5, 8),
        PlacedDigit(2, 6, 9),

        PlacedDigit(6, 4, 6),
        PlacedDigit(5, 5, 7),
        PlacedDigit(4, 6, 4),

        PlacedDigit(8, 4, 3),
        PlacedDigit(8, 5, 5),
        PlacedDigit(7, 6, 1),

        PlacedDigit(3, 7, 4),
        PlacedDigit(2, 8, 1),
        PlacedDigit(1, 9, 6),

        PlacedDigit(5, 8, 6),
        PlacedDigit(6, 8, 7),
        PlacedDigit(4, 9, 5),

        PlacedDigit(7, 7, 6),
        PlacedDigit(9, 7, 3),
        PlacedDigit(8, 8, 9),
        PlacedDigit(9, 9, 8)
      ))

    val solved = SudokuSolver.solve(state.get)
    println(solved.get.placedDigits)
    (state.get.analysis.isCompletable must beTrue) and
    (solved must beSome)
  } ^ end
}
