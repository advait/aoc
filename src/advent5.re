open Batteries;

let lines = File.lines_of("../test.txt") |> BatList.of_enum;
assert(lines |> BatList.length == 1);
let polymer = lines |> BatList.hd |> BatString.to_list;

/* Returns true whether a and b are polar elements. */
let are_polar = (a: char, b: char): bool =>
  if (BatChar.lowercase(a) != BatChar.lowercase(b)) {
    false;
  } else {
    BatChar.is_lowercase(a)
    && BatChar.is_uppercase(b)
    || BatChar.is_uppercase(a)
    && BatChar.is_lowercase(b);
  };

let rec react_once = (l: list(char)): (list(char), bool) => {
  switch (l) {
  | [a, b, ...tail] when are_polar(a, b) => (tail, true)
  | [a, ...tail] =>
    let (new_tail, reacted) = react_once(tail);
    ([a, ...new_tail], reacted);
  | [] => ([], false)
  };
};

let rec react_all = (l: list(char)): list(char) => {
  let (l_, reacted) = react_once(l);
  if (!reacted) {
    l;
  } else {
    react_all(l_);
  };
};

let string_of_char_list = (l: list(char)): string => {
  l |> BatList.map(BatString.of_char) |> BatString.join("");
};

/* Part one */
print_endline(
  "Length: " ++ string_of_int(react_all(polymer) |> BatList.length),
);

/* Part Two */

let alphabet =
  BatEnum.(int_of_char('a') -- int_of_char('z'))
  |> BatEnum.map(char_of_int)
  |> BatList.of_enum;

/* Filters the polymer of the given letter */
let filter_polymer = (letter: char, l: list(char)): list(char) => {
  l |> BatList.filter(c => BatChar.lowercase(c) != BatChar.lowercase(letter));
};

let filtered_polymers =
  alphabet |> BatList.map(letter => filter_polymer(letter, polymer));

let smallest_polymer =
  filtered_polymers
  |> BatList.map(polymer => react_all(polymer) |> BatList.length)
  |> BatList.min;

print_endline("Smallest polymer: " ++ string_of_int(smallest_polymer));