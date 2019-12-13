let digits_of_int = number =>
  number
  |> string_of_int
  |> Js.String.split("")
  |> Array.map(int_of_string)
  |> Array.to_list;

let neverDecrease = {
  let rec neverDecreaseHelper = (previous, digits) =>
    switch (digits) {
    | [] => true
    | [head, ...tail] =>
      if (previous > head) {
        false;
      } else {
        neverDecreaseHelper(head, tail);
      }
    };

  neverDecreaseHelper(-1);
};

let hasRepetition = {
  let rec hasRepetitionHelper = (previous, digits) =>
    switch (digits) {
    | [] => false
    | [head, ...tail] =>
      if (previous == head) {
        true;
      } else {
        hasRepetitionHelper(head, tail);
      }
    };

  hasRepetitionHelper(-1);
};

let hasTwoRepeated = {
  let rec hasTwoRepeatedHelper = (numberOfRepetitions, previous, digits) => {
    switch (digits) {
    | [] => numberOfRepetitions === 2
    | [head, ...tail] =>
      if (previous == head) {
        hasTwoRepeatedHelper(numberOfRepetitions + 1, head, tail);
      } else if (numberOfRepetitions === 2) {
        true;
      } else {
        hasTwoRepeatedHelper(1, head, tail);
      }
    };
  };

  hasTwoRepeatedHelper(1, -1);
};

let meetsCriteria = number => {
  let input = digits_of_int(number);

  neverDecrease(input) && hasRepetition(input);
};

let meetsCriteriaWithoutGroups = number => {
  let input = digits_of_int(number);

  neverDecrease(input) && hasTwoRepeated(input);
};

let countInRange = (criteria, rangeStart, rangeEnd) => {
  let count = ref(0);

  for (i in rangeStart to rangeEnd) {
    if (criteria(i)) {
      count := count^ + 1;
    };
  };

  count^;
};
