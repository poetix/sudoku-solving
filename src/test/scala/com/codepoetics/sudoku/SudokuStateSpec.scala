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
  "updates its analysis when a digit is added to it" ! {
    val analysis = SudokuAnalysis()
    val state = SudokuState(Nil, analysis)
    val digit = PlacedDigit(1, 1, 1)

    state.after(digit).get.analysis must beEqualTo(analysis.after(digit).get)
  } ^ end
}
