let input = [];
/* Put the input here as an array of numbers :)~ */

let getFuelRequired = mass => mass / 3 - 2;

let rec sumAll = elements =>
  switch (elements) {
  | [a, ...rest] => a + sumAll(rest)
  | [] => 0
  };

let sumAllRequirements = masses => sumAll(List.map(getFuelRequired, masses));

let rec sumAllRequirementsWithFuel = masses => {
  let fuels =
    List.map(getFuelRequired, masses) |> List.filter(value => value > 0);

  if (masses != []) {
    sumAll(fuels) + sumAllRequirementsWithFuel(fuels);
  } else {
    0;
  };
};
