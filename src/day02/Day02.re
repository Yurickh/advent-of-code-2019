let extractInstruction = (index, array) =>
  switch (Array.sub(array, index, 4)) {
  | [|_, p1, p2, t|] => (p1, p2, t)
  | _ => (0, 0, 0)
  };

let runSum = (index, array) => {
  let (param1, param2, target) = extractInstruction(index, array);
  array[target] = array[param1] + array[param2];
};

let runMul = (index, array) => {
  let (param1, param2, target) = extractInstruction(index, array);
  array[target] = array[param1] * array[param2];
};

let runProgram = input => {
  let current = ref(0);

  while (current^ < Array.length(input) && input[current^] !== 99) {
    switch (input[current^]) {
    | 1 => runSum(current^, input)
    | 2 => runMul(current^, input)
    | _ => ()
    };
    current := current^ + 4;
  };

  input;
};

let runProgramWithInput = (override, input) => {
  Array.concat([
    [|input[0]|],
    override,
    Js.Array.slice(~start=Array.length(override) + 1, ~end_=-1, input),
  ])
  |> runProgram;
};

let findValueWithOutput = (input, expected) => {
  let found = ref(((-1), (-1)));

  for (noun in 0 to 99) {
    for (verb in 0 to 99) {
      if (runProgramWithInput([|noun, verb|], input)[0] === expected) {
        found := (noun, verb);
      };
    };
  };

  found^;
};
