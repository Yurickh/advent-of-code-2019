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
