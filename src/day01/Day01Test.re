open Jest;
open Expect;

describe("Day 1: The Tyranny of the Rocket Equation", () => {
  describe("Part One", () => {
    test("works with the example", () =>
      expect(Day01.sumAllRequirements([1969, 12])) |> toBe(654 + 2)
    );

    test("solves the problem", () =>
      expect(Day01.sumAllRequirements(Day01DataSet.input)) |> toBe(3256599)
    );
  });

  describe("Part Two", () => {
    test("works with the example", () =>
      expect(Day01.sumAllRequirementsWithFuel([100756, 14]))
      |> toBe(50346 + 2)
    );

    test("solves the problem", () =>
      expect(Day01.sumAllRequirementsWithFuel(Day01DataSet.input))
      |> toBe(4882038)
    );
  });
});
