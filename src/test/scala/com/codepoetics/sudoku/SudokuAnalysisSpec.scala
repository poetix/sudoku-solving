package com.codepoetics.sudoku

import org.specs2.Specification
import Types._

class SudokuAnalysisSpec extends Specification {
  def is =
  "A SudokuAnalysis" ^
    "should remove a cell from the free cells when a digit is placed in it" ! {
      SudokuAnalysis().after(PlacedDigit(3, 4, 1)).get.freeCells must beEqualTo(List(
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup,
        DigitGroup(1, 2, 4, 5, 6, 7, 8, 9),
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup
      ))
    } ^
    "should remove a digit from the free digits by row when a digit is placed" ! {
      SudokuAnalysis().after(PlacedDigit(3, 4, 1)).get.freeDigitsByRow must beEqualTo(List(
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup,
        DigitGroup(2, 3, 4, 5, 6, 7, 8, 9),
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup
      ))
    } ^
    "should remove a digit from the free digits by column when a digit is placed" ! {
      SudokuAnalysis().after(PlacedDigit(3, 4, 7)).get.freeDigitsByCol must beEqualTo(List(
        fullDigitGroup,
        fullDigitGroup,
        DigitGroup(1, 2, 3, 4, 5, 6, 8, 9),
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup
      ))
    } ^
    "should remove a digit from the free digits by subgrid when a digit is placed" ! {
      (PlacedDigit(3, 4, 7).subgrid must beEqualTo(4)) and
      (SudokuAnalysis().after(PlacedDigit(3, 4, 7)).get.freeDigitsBySubGrid must beEqualTo(List(
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup,
        DigitGroup(1, 2, 3, 4, 5, 6, 8, 9),
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup,
        fullDigitGroup
      )))
    } ^
    "should remove a cell from the possible moves when a digit is placed in it" ! {
      SudokuAnalysis().after(PlacedDigit(6, 4, 1)).get.possibleMoves.find( pd => pd.row == 4 && pd.col == 6) must beNone
    } ^ end
}
