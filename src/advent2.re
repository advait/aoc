open Batteries;

let lines = File.lines_of("../test.txt") |> BatList.of_enum;

module CharMap = BatMap.Char;

/* Given a string return a map of char counts */
let get_char_map = (s: string): CharMap.t(int) => {
  let rec temp = (s: string, acc: CharMap.t(int)) => {
    switch (s) {
    | "" => acc
    | s =>
      let c = s.[0];
      let acc_ =
        switch (CharMap.find(c, acc)) {
        | count => CharMap.add(c, count + 1, acc)
        | exception Not_found => CharMap.add(c, 1, acc)
        };
      let s_ = String.sub(s, 1, String.length(s) - 1);
      temp(s_, acc_);
    };
  };
  temp(s, CharMap.empty);
};

let has_n = (n: 'a, m: CharMap.t('a)): int =>
  if (CharMap.exists((_, count) => n == count, m)) {
    1;
  } else {
    0;
  };

let has_two_three = (m: CharMap.t('a)): (int, int) => {
  (has_n(2, m), has_n(3, m));
};

let tp = (a: (int, int), b: (int, int)): (int, int) => {
  let (a1, a2) = a;
  let (b1, b2) = b;
  (a1 + b1, a2 + b2);
};

let counts = lines |> BatList.map(get_char_map);

let (twos, threes) =
  counts
  |> BatList.fold_left((acc, m) => tp(acc, has_two_three(m)), (0, 0));

print_endline(twos |> string_of_int);
print_endline(threes |> string_of_int);
print_endline(twos * threes |> string_of_int);

/* Given two box IDs return the number of chars that differ from each */
let rec comp_boxes = (a: string, b: string): int =>
  if (a == "" || b == "") {
    0;
  } else {
    let a_ = BatString.sub(a, 1, BatString.length(a) - 1);
    let b_ = BatString.sub(b, 1, BatString.length(b) - 1);
    let remaining = comp_boxes(a_, b_);
    a.[0] == b.[0] ? remaining : 1 + remaining;
  };

let sorted_lines = BatList.sort(BatString.compare, lines);

let rec one_comp = (lines: list(string)): (string, string) => {
  switch (lines) {
  | [a, b, ...tail] =>
    let diff = comp_boxes(a, b);
    if (diff == 1) {
      (a, b);
    } else {
      one_comp([b, ...tail]);
    };
  | _ => raise(Not_found)
  };
};

print_endline(dump(sorted_lines |> one_comp));