open Batteries;

let lines = File.lines_of("../test.txt") |> BatList.of_enum;

let re2 = Re.Perl.compile_pat({|^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$|});

/* Represents an elf claim on the magic monolithic cloth */
type claim = {
  id: int,
  x: int,
  y: int,
  width: int,
  height: int,
};

/* Use regex to parse a string into a claim */
let string_to_claim = (s: string): claim => {
  let groups = Re.Pcre.extract(~rex=re2, s);
  {
    id: int_of_string(groups[1]),
    x: int_of_string(groups[2]),
    y: int_of_string(groups[3]),
    width: int_of_string(groups[4]),
    height: int_of_string(groups[5]),
  };
};

let claims = lines |> List.map(string_to_claim);

type square = (int, int);

module SquareMap =
  Map.Make({
    type t = square;
    let compare = compare;
  });

/* Return a list with values [start, start+len) */
let n_iter = (start: int, len: int) =>
  BatEnum.(start -- (start + len - 1)) |> BatList.of_enum;

/* Given a claim return a list of squares it covers */
let claim_to_squares = (c: claim): list(square) => {
  let xs = n_iter(c.x, c.width);
  let ys = n_iter(c.y, c.height);
  xs |> List.map(x => ys |> List.map(y => (x, y))) |> List.flatten;
};

let square_counts: SquareMap.t(int) =
  claims
  |> List.map(claim_to_squares)
  |> List.flatten
  |> List.fold_left(
       (m, s: square): SquareMap.t(int) =>
         switch (m |> SquareMap.find(s)) {
         | c => m |> SquareMap.add(s, c + 1)
         | exception Not_found => m |> SquareMap.add(s, 1)
         },
       SquareMap.empty,
     );

let overlapping_squares =
  square_counts
  |> SquareMap.filter((_, count) => count >= 2)
  |> SquareMap.cardinal;

print_endline(overlapping_squares |> string_of_int);

/* Part 2: Figure out which claim doesn't overlap with anything */

let nonoverlapping_claims =
  claims
  |> List.filter(claim =>
       claim
       |> claim_to_squares
       |> List.for_all(square =>
            switch (square_counts |> SquareMap.find(square)) {
            | 1 => true
            | _ => false
            }
          )
     );
print_endline(dump(nonoverlapping_claims));