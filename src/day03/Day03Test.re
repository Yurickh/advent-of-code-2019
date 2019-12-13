open Jest;
open Expect;

describe("Day 3: Crossed Wires", () => {
  describe("Part 01", () => {
    describe("works with the example", () => {
      test("basic", () =>
        expect(Day03.shortestPath(("R8,U5,L5,D3", "U7,R6,D4,L4")))
        |> toBe(6)
      );

      test("1", () =>
        expect(
          Day03.shortestPath((
            "R75,D30,R83,U83,L12,D49,R71,U7,L72",
            "U62,R66,U55,R34,D71,R55,D58,R83",
          )),
        )
        |> toBe(159)
      );

      test("2", () =>
        expect(
          Day03.shortestPath((
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
            "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7",
          )),
        )
        |> toBe(135)
      );
    });

    test("solves the problem", () =>
      expect(Day03.shortestPath(Day03DataSet.input)) |> toBe(1626)
    );
  });

  describe("Part 02", () => {
    describe("works with the example", () => {
      test("basic", () =>
        expect(Day03.countSteps(("R8,U5,L5,D3", "U7,R6,D4,L4"))) |> toBe(30)
      );

      test("1", () =>
        expect(
          Day03.countSteps((
            "R75,D30,R83,U83,L12,D49,R71,U7,L72",
            "U62,R66,U55,R34,D71,R55,D58,R83",
          )),
        )
        |> toBe(610)
      );

      test("2", () =>
        expect(
          Day03.countSteps((
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
            "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7",
          )),
        )
        |> toBe(410)
      );
    });

    test("solves the problem", () =>
      expect(Day03.countSteps(Day03DataSet.input)) |> toBe(27330)
    );
  });
});
