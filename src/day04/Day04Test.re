open Jest;
open Expect;

describe("Day 4: Secure Container", () => {
  describe("Part 01", () => {
    describe("works with the example", () => {
      test("basic", () =>
        expect(Day04.meetsCriteria(111111)) |> toBe(true)
      );

      test("never decreases", () =>
        expect(Day04.meetsCriteria(223450)) |> toBe(false)
      );

      test("no double", () =>
        expect(Day04.meetsCriteria(123789)) |> toBe(false)
      );
    });

    test("solves the problem", () =>
      expect(Day04.countInRange(Day04.meetsCriteria, 138241, 674034))
      |> toBe(1890)
    );
  });

  describe("Part 02", () => {
    test("basic", () =>
      expect(Day04.meetsCriteriaWithoutGroups(112233)) |> toBe(true)
    );

    test("dont work with groups", () =>
      expect(Day04.meetsCriteriaWithoutGroups(123444)) |> toBe(false)
    );

    test("takes into account different groups", () =>
      expect(Day04.meetsCriteriaWithoutGroups(111122)) |> toBe(true)
    );

    test("does not count groups of 4", () =>
      expect(Day04.meetsCriteriaWithoutGroups(555589)) |> toBe(false)
    );

    test("solves the problem", () =>
      expect(
        Day04.countInRange(Day04.meetsCriteriaWithoutGroups, 138241, 674034),
      )
      |> toBe(1277)
    );
  });
});
