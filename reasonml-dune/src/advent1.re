open Batteries;

let lines = File.lines_of("../test.txt");

let ints = lines |> BatEnum.map(int_of_string);
let infinite_ints = ints |> BatEnum.clone |> BatEnum.cycle;

print_endline("Sum: " ++ (ints |> BatEnum.sum |> string_of_int));

let rec repeats = (cur: int, visited: BatSet.Int.t, l: BatEnum.t(int)): int => {
  if (BatSet.Int.mem(cur, visited)) {
    cur;
  } else {
    let visited_ = visited |> BatSet.Int.add(cur);
    let next = l |> BatEnum.get |> BatOption.get;
    repeats(cur + next, visited_, l);
  };
};

print_endline("Repeat: " ++ (repeats(0, BatSet.Int.empty, infinite_ints) |> string_of_int))