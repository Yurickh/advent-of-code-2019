open Jest;
open Expect;

let expectToOutput = (fn, _value) => {
  // Mocking is extremely limited within bs-jest
  JestJs.resetAllMocks();
  let _mock = [%raw "console.log = jest.fn()"];
  let _result = fn();
  let _expect = [%raw "expect(console.log).toHaveBeenCalledWith(_value)"];
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
  })
});
