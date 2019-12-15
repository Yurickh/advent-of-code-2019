type opcode =
  | Sum
  | Mul
  | Output
  | Input
  | JumpIfTrue
  | JumpIfFalse
  | LessThan
  | Equals
  | None;

let opcode_of_int = int =>
  switch (int) {
  | 1 => Sum
  | 2 => Mul
  | 3 => Input
  | 4 => Output
  | 5 => JumpIfTrue
  | 6 => JumpIfFalse
  | 7 => LessThan
  | 8 => Equals
  | _ => None
  };

let rec padStart = (list, value, length) => {
  switch (length - List.length(list)) {
  | 0 => list
  | _ => padStart([value, ...list], value, length)
  };
};

module InstructionParameter = {
  type mode =
    | Immediate
    | Position
    | None;

  type access =
    | Get
    | Set
    | None;

  let mode_of_int = int =>
    switch (int) {
    | 0 => Position
    | 1 => Immediate
    | _ => None
    };

  let mode_to_string = mode =>
    switch (mode) {
    | Position => "Position"
    | Immediate => "Immediate"
    | None => "None"
    };

  let accessType_of_opcode = opcode =>
    switch (opcode) {
    | Sum => [Get, Get, Set]
    | Mul => [Get, Get, Set]
    | Input => [Set]
    | Output => [Get]
    | JumpIfTrue => [Get, Get]
    | JumpIfFalse => [Get, Get]
    | LessThan => [Get, Get, Set]
    | Equals => [Get, Get, Set]
    | None => []
    };

  let from_instructionSet = (baseAddress, opcode, instructionSet) => {
    Array.make(List.length(accessType_of_opcode(opcode)), 0)
    |> Array.mapi((index, _) => instructionSet[baseAddress + 1 + index])
    |> Array.to_list;
  };

  let getValueWithMode = ((mode, parameter), instructionSet) => {
    switch (mode) {
    | Position => instructionSet[parameter]
    | Immediate => parameter
    | None => 0
    };
  };

  let makePairList = (parameterModes, parameterValues) => {
    List.map2(
      (mode, param) => (mode, param),
      List.rev(
        padStart(
          List.rev(parameterModes),
          Position,
          List.length(parameterValues),
        ),
      ),
      parameterValues,
    );
  };

  let extract = (opcode, parameterWithModes, instructionSet) => {
    List.map2(
      (accessType, (mode, parameter)) => {
        switch (accessType) {
        | Get => getValueWithMode((mode, parameter), instructionSet)
        | Set => parameter
        | None => (-1)
        }
      },
      accessType_of_opcode(opcode),
      parameterWithModes,
    )
    |> Array.of_list;
  };
};

let instruction_of_int = int => {
  let opcode = opcode_of_int(int mod 100);
  let parameterModeValues =
    Day04.digits_of_int(int / 100)
    |> List.rev
    |> List.map(InstructionParameter.mode_of_int);

  (opcode, parameterModeValues);
};

let runSum = (parameters, instructionSet) => {
  instructionSet[parameters[2]] = parameters[0] + parameters[1];
  4;
};

let runMul = (parameters, instructionSet) => {
  instructionSet[parameters[2]] = parameters[0] * parameters[1];
  4;
};

let runInput = (parameters, instructionSet, input) => {
  instructionSet[parameters[0]] = input;
  2;
};

let runOutput = parameters => {
  Js.log(parameters[0]);
  2;
};

let runJumpIfTrue = (parameters, address) =>
  if (parameters[0] !== 0) {
    parameters[1] - address;
  } else {
    3;
  };

let runJumpIfFalse = (parameters, address) =>
  if (parameters[0] === 0) {
    parameters[1] - address;
  } else {
    3;
  };

let runLessThan = (parameters, instructionSet) => {
  instructionSet[parameters[2]] = parameters[0] < parameters[1] ? 1 : 0;
  4;
};

let runEquals = (parameters, instructionSet) => {
  instructionSet[parameters[2]] = parameters[0] === parameters[1] ? 1 : 0;
  4;
};

let executeInstruction = (address, instructionSet, input) => {
  let (opcode, parameterModes) = instruction_of_int(instructionSet[address]);

  let parameterValues =
    InstructionParameter.from_instructionSet(address, opcode, instructionSet);

  let parametersWithModes =
    InstructionParameter.makePairList(parameterModes, parameterValues);

  let parameters =
    InstructionParameter.extract(opcode, parametersWithModes, instructionSet);

  switch (opcode) {
  | Sum => runSum(parameters, instructionSet)
  | Mul => runMul(parameters, instructionSet)
  | Input => runInput(parameters, instructionSet, input)
  | Output => runOutput(parameters)
  | JumpIfTrue => runJumpIfTrue(parameters, address)
  | JumpIfFalse => runJumpIfFalse(parameters, address)
  | LessThan => runLessThan(parameters, instructionSet)
  | Equals => runEquals(parameters, instructionSet)
  | _ => 1
  };
};

let runProgram = (instructionSet, input) => {
  let current = ref(0);

  while (current^ < Array.length(instructionSet)
         && instructionSet[current^] !== 99) {
    current := current^ + executeInstruction(current^, instructionSet, input);
  };

  instructionSet;
};
