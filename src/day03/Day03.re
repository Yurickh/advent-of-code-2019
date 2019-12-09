type coordinate = {
  x: int,
  y: int,
};

type instruction =
  | L(int)
  | R(int)
  | U(int)
  | D(int);

let origin = {x: 0, y: 0};

let rec executeInstruction = (origin, instruction) =>
  switch (instruction) {
  | U(0)
  | D(0)
  | L(0)
  | R(0) => [origin]
  | U(amount) => [
      origin,
      ...executeInstruction({x: origin.x, y: origin.y + 1}, U(amount - 1)),
    ]
  | D(amount) => [
      origin,
      ...executeInstruction({x: origin.x, y: origin.y - 1}, D(amount - 1)),
    ]
  | L(amount) => [
      origin,
      ...executeInstruction({x: origin.x - 1, y: origin.y}, L(amount - 1)),
    ]
  | R(amount) => [
      origin,
      ...executeInstruction({x: origin.x + 1, y: origin.y}, R(amount - 1)),
    ]
  };

let separateLast = list =>
  switch (List.rev(list)) {
  | [last, ...rest] => (last, List.rev(rest))
  | [] => (origin, [])
  };

let rec drawPath = (origin, instructionSet) =>
  switch (instructionSet) {
  | [] => [origin]
  | [currentInstruction, ...rest] =>
    let (last, line) =
      executeInstruction(origin, currentInstruction) |> separateLast;

    List.flatten([line, drawPath(last, rest)]);
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
  Array.to_list(
    Array.map(instruction_of_string, Js.String.split(",", input)),
  );

let intersections = listB =>
  List.filter(elA => List.exists(elB => elA == elB, listB));

let shortestPath = ((inputA, inputB)) => {
  let pathA = drawPath(origin, instructionSet_of_input(inputA));
  let pathB = drawPath(origin, instructionSet_of_input(inputB));

  intersections(pathA, pathB)
  |> List.filter(element => element != origin)
  |> List.map(coordinate => abs(coordinate.x) + abs(coordinate.y))
  |> Array.of_list
  |> Js.Math.minMany_int;
};
