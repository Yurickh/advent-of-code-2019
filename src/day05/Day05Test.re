open Jest;
open Expect;

let expectToOutput = (_fn, _value) => {
  // Mocking is extremely limited within bs-jest
  let _mock = [%raw
    {|(() => {
      const mock = console.log
      console.log = jest.fn()
      _fn()
      expect(console.log).toHaveBeenCalledWith(_value)
      console.log = mock
    })()|}
  ];
  expect(true) |> toEqual(true);
};

describe("Day 5: Sunny with a Chance of Asteroids", () => {
  describe("Part One", () => {
    describe("works with the example", () => {
      test("base", () =>
        expect(
          Day05.runProgram([|1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50|], 0),
        )
        |> toEqual([|3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50|])
      );

      test("sum", () =>
        expect(Day05.runProgram([|1, 0, 0, 0, 99|], 0))
        |> toEqual([|2, 0, 0, 0, 99|])
      );

      test("multiplication", () =>
        expect(Day05.runProgram([|2, 3, 0, 3, 99|], 0))
        |> toEqual([|2, 3, 0, 6, 99|])
      );

      test("complex multiplication", () =>
        expect(Day05.runProgram([|2, 4, 4, 5, 99, 0|], 0))
        |> toEqual([|2, 4, 4, 5, 99, 9801|])
      );

      test("input", () =>
        expect(Day05.runProgram([|3, 0, 99|], 35))
        |> toEqual([|35, 0, 99|])
      );

      test("ouput", () => {
        expectToOutput(() => Day05.runProgram([|4, 0, 99|], 0), 4)
      });

      test("with combination of operations", () =>
        expect(Day05.runProgram([|1, 1, 1, 4, 99, 5, 6, 0, 99|], 0))
        |> toEqual([|30, 1, 1, 4, 2, 5, 6, 0, 99|])
      );
    });

    test("solves the problem", () => {
      expectToOutput(
        () => Day05.runProgram(Day05DataSet.instructionSet, 1),
        13294380,
      )
    });
  });

  describe("Part Two", () => {
    describe("works with the example", () => {
      test("when input equals 8", () =>
        expectToOutput(
          () =>
            Day05.runProgram([|3, 9, 8, 9, 10, 9, 4, 9, 99, (-1), 8|], 8),
          1,
        )
      );
      test("when input does not equal 8", () =>
        expectToOutput(
          () =>
            Day05.runProgram([|3, 9, 8, 9, 10, 9, 4, 9, 99, (-1), 8|], 5),
          0,
        )
      );
      test("when input is less than 8", () =>
        expectToOutput(
          () =>
            Day05.runProgram([|3, 9, 7, 9, 10, 9, 4, 9, 99, (-1), 8|], 5),
          1,
        )
      );
      test("when input is more than 8", () =>
        expectToOutput(
          () =>
            Day05.runProgram([|3, 9, 7, 9, 10, 9, 4, 9, 99, (-1), 8|], 10),
          0,
        )
      );
      test("when input equals 8 (immediate)", () =>
        expectToOutput(
          () => Day05.runProgram([|3, 3, 1108, (-1), 8, 3, 4, 3, 99|], 8),
          1,
        )
      );
      test("when input does not equal 8 (immediate)", () =>
        expectToOutput(
          () => Day05.runProgram([|3, 3, 1108, (-1), 8, 3, 4, 3, 99|], 5),
          0,
        )
      );
      test("with combination of operations", () =>
        expect(Day05.runProgram([|1, 1, 1, 4, 99, 5, 6, 0, 99|], 0))
        |> toEqual([|30, 1, 1, 4, 2, 5, 6, 0, 99|])
      );
      test("when input is less than 8", () =>
        expectToOutput(
          () => Day05.runProgram([|3, 3, 1107, (-1), 8, 3, 4, 3, 99|], 5),
          1,
        )
      );
      test("when input is more than 8", () =>
        expectToOutput(
          () => Day05.runProgram([|3, 3, 1107, (-1), 8, 3, 4, 3, 99|], 10),
          0,
        )
      );
      test("when input is zero", () =>
        expectToOutput(
          () =>
            Day05.runProgram(
              [|3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, (-1), 0, 1, 9|],
              0,
            ),
          0,
        )
      );
      test("when input is non-zero", () =>
        expectToOutput(
          () =>
            Day05.runProgram(
              [|3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, (-1), 0, 1, 9|],
              27,
            ),
          1,
        )
      );
      test("when input is zero (immediate)", () =>
        expectToOutput(
          () =>
            Day05.runProgram(
              [|3, 3, 1105, (-1), 9, 1101, 0, 0, 12, 4, 12, 99, 1|],
              0,
            ),
          0,
        )
      );
      test("when input is non-zero (immediate)", () =>
        expectToOutput(
          () =>
            Day05.runProgram(
              [|3, 3, 1105, (-1), 9, 1101, 0, 0, 12, 4, 12, 99, 1|],
              27,
            ),
          1,
        )
      );
      test("when input is less than eight (complex)", () =>
        expectToOutput(
          () => Day05.runProgram(Day05DataSet.complexExample, 2),
          999,
        )
      );
      test("when input is more than eight (complex)", () =>
        expectToOutput(
          () => Day05.runProgram(Day05DataSet.complexExample, 27),
          1001,
        )
      );
      test("when input equals eight (complex)", () =>
        expectToOutput(
          () => Day05.runProgram(Day05DataSet.complexExample, 8),
          1000,
        )
      );
    });

    test("solves the problem", () => {
      expectToOutput(
        () => Day05.runProgram(Day05DataSet.instructionSet, 5),
        11460760,
      )
    });
  });
});
