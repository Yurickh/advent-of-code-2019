type opcode =
  | Sum
  | Mul
  | Output
  | Input
  | None;

let opcode_of_int = int =>
  switch (int) {
  | 1 => Sum
  | 2 => Mul
  | 3 => Input
  | 4 => Output
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
    | Sum
    | Mul => [Get, Get, Set]
    | Input => [Set]
    | Output => [Get]
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

let nth_or = (defaultValue, list, index) =>
  try(List.nth(list, index)) {
  | Failure("nth") => defaultValue
  };

let a1 = (cb, a, _) => cb(a);

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
};

let runMul = (parameters, instructionSet) => {
  instructionSet[parameters[2]] = parameters[0] * parameters[1];
};

let runInput = (parameters, instructionSet, input) => {
  instructionSet[parameters[0]] = input;
};

let runOutput = parameters => {
  Js.log(parameters[0]);
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
  | _ => ()
  };

  1 + List.length(parameterValues);
};

let runProgram = (instructionSet, input) => {
  let current = ref(0);

  while (current^ < Array.length(instructionSet)
         && instructionSet[current^] !== 99) {
    current := current^ + executeInstruction(current^, instructionSet, input);
  };

  instructionSet;
};
