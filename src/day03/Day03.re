type coordinate = {
  x: int,
  y: int,
};

module CoordinateSet = {
  module CoordinateComparator =
    Belt.Id.MakeComparable({
      type t = coordinate;
      let cmp = (c1, c2) => {
        let compareX = compare(c1.x, c2.x);
        compareX == 0 ? compare(c1.y, c2.y) : compareX;
      };
    });

  let of_array = Belt.Set.fromArray(~id=(module CoordinateComparator));
  let of_list = list => list |> Array.of_list |> of_array;
};

type instruction =
  | L(int)
  | R(int)
  | U(int)
  | D(int);

let origin = {x: 0, y: 0};

let executeInstruction = {
  let rec executeInstructionHelper = (list, reference, instruction) => {
    switch (instruction) {
    | U(0)
    | D(0)
    | L(0)
    | R(0) => [reference, ...list]
    | U(amount) =>
      executeInstructionHelper(
        [reference, ...list],
        {x: reference.x, y: reference.y + 1},
        U(amount - 1),
      )
    | D(amount) =>
      executeInstructionHelper(
        [reference, ...list],
        {x: reference.x, y: reference.y - 1},
        D(amount - 1),
      )
    | L(amount) =>
      executeInstructionHelper(
        [reference, ...list],
        {x: reference.x - 1, y: reference.y},
        L(amount - 1),
      )
    | R(amount) =>
      executeInstructionHelper(
        [reference, ...list],
        {x: reference.x + 1, y: reference.y},
        R(amount - 1),
      )
    };
  };

  executeInstructionHelper([]);
};

let rec spreadInFront = (butter, bread) => {
  switch (butter) {
  | [] => bread
  | [head, ...tail] => spreadInFront(tail, [head, ...bread])
  };
};

let drawPath = {
  let rec drawPathHelper = (coordinates, reference, instructionSet) => {
    switch (instructionSet) {
    | [] => [reference, ...coordinates]
    | [currentInstruction, ...rest] =>
      switch (executeInstruction(reference, currentInstruction)) {
      | [] => []
      | [last, ...line] =>
        drawPathHelper(
          spreadInFront(List.rev(line), coordinates),
          last,
          rest,
        )
      }
    };
  };

  drawPathHelper([], origin);
};

let splitInstruction = instructionAsString => {
  switch (Array.to_list(Js.String.split("", instructionAsString))) {
  | [direction, ...number] => (
      direction,
      int_of_string(Js.Array.joinWith("", Array.of_list(number))),
    )
  | [] => ("U", 0)
  };
};

let instruction_of_string = str =>
  switch (splitInstruction(str)) {
  | ("U", amount) => U(amount)
  | ("D", amount) => D(amount)
  | ("L", amount) => L(amount)
  | ("R", amount) => R(amount)
  | _ => U(0)
  };

let instructionSet_of_input = input =>
  List.map(
    instruction_of_string,
    Array.to_list(Js.String.split(",", input)),
  );

let intersections = (listA, listB) => {
  let itemsA = CoordinateSet.of_list(listA);
  let itemsB = CoordinateSet.of_list(listB);

  Belt.Set.intersect(itemsA, itemsB) |> Belt.Set.toArray |> Array.to_list;
};

let shortestPath = ((inputA, inputB)) => {
  let pathA = drawPath(instructionSet_of_input(inputA));
  let pathB = drawPath(instructionSet_of_input(inputB));

  intersections(pathA, pathB)
  |> List.filter(element => element != origin)
  |> List.map(coordinate => abs(coordinate.x) + abs(coordinate.y))
  |> Array.of_list
  |> Js.Math.minMany_int;
};
