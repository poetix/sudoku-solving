package com.codepoetics.sudoku

import org.specs2.Specification
import Types._

class SudokuStateSpec extends Specification {
  def is =
  "A SudokuState" ^
    "should be empty on initialisation" ! {
      SudokuState() must beEmpty
  } ^
  "should be able to have a placed digit added to it" ! {
    SudokuState().after(PlacedDigit(1, 1, 1)) must not(beEmpty)
  } ^
  "is complete when 81 digits have been added to it" ! {
    val digits = for {
      col <- (1 to 9)
      row <- (1 to 9)
    } yield PlacedDigit(col, row, 1)
    SudokuState().afterAll(digits).isComplete must beTrue
  } ^
  "updates its analysis when a digit is added to it" ! {
    val analysis = SudokuAnalysis()
    val state = SudokuState(Nil, analysis)
    val digit = PlacedDigit(1, 1, 1)

    println(state.after(digit).analysis.possibleMoves.take(10).toList)

    state.after(digit).analysis must beEqualTo(analysis.after(digit))
  } ^
  "Solves an empty puzzle" ! {
      val analysis = SudokuAnalysis()
      val state = SudokuState(Nil, analysis)
      val digit = PlacedDigit(1, 1, 1)

      println(SudokuSolver.solve(state.after(digit)).get.placedDigits)
    state.after(digit).analysis must beEqualTo(analysis.after(digit))
    } ^ end
}
