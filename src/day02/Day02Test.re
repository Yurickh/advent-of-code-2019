open Jest;
open Expect;

describe("Day 2: 1202 Program Alarm", () => {
  describe("Part One", () => {
    describe("works with the example", () => {
      test("base", () =>
        expect(
          Day02.runProgram([|1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50|]),
        )
        |> toEqual([|3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50|])
      );

      test("sum", () =>
        expect(Day02.runProgram([|1, 0, 0, 0, 99|]))
        |> toEqual([|2, 0, 0, 0, 99|])
      );

      test("multiplication", () =>
        expect(Day02.runProgram([|2, 3, 0, 3, 99|]))
        |> toEqual([|2, 3, 0, 6, 99|])
      );

      test("complex multiplication", () =>
        expect(Day02.runProgram([|2, 4, 4, 5, 99, 0|]))
        |> toEqual([|2, 4, 4, 5, 99, 9801|])
      );

      test("with combination of operations", () =>
        expect(Day02.runProgram([|1, 1, 1, 4, 99, 5, 6, 0, 99|]))
        |> toEqual([|30, 1, 1, 4, 2, 5, 6, 0, 99|])
      );
    });

    test("solves the problem", () => {
      expect(Day02.runProgram(Day02DataSet.input)[0]) |> toBe(5866663)
    });
  })
});
