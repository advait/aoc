/* Start Helper feunctions - Advait's hacked-together stdlib. */
/* TODO(advait): I wish I could read from stdin... */
let text = Node_fs.readFileSync("test.txt", `utf8);

let re = Js.Re.fromStringWithFlags("(^.+$)", ~flags="gm");

/* Generator */
type gen('a) = unit => 'a;

let map_gen = (f: 'a => 'b, g: gen('a)): gen('b) => {
  () => f(g());
};

let rec fold_gen = (f: ('a, 'b) => 'a, start: 'a, g: gen(option('b))) => {
  switch (g()) {
  | None => start
  | Some(b) => fold_gen(f, f(start, b), g)
  };
};

let rec foreach_gen = (f: 'a => unit, g: gen(option('a))) => {
  switch (g()) {
  | None => ()
  | Some(a) =>
    f(a);
    foreach_gen(f, g);
  };
};

let gen_lines = (): gen(option(string)) => {
  () => {
    switch (Js.Re.exec(text, re)) {
    | Some(r) => Js.Re.captures(r)[0] |> Js.Nullable.toOption
    | _ => None
    };
  };
};

let gen_to_list = (g: gen(option('a))): list('a) => {
  let rec aux = (acc): list('a) => {
    switch (g()) {
    | None => acc
    | Some(a) => aux([a, ...acc])
    };
  };
  List.rev(aux([]));
};

let list_to_gen = (l: list('a)): gen(option('a)) => {
  let r = ref(l);
  () => {
    switch (r^) {
    | [] => None
    | [head, ...tail] =>
      r := tail;
      Some(head);
    };
  };
};

let lines = gen_lines() |> gen_to_list;

/* End Helper Functions */

let re2 = Js.Re.fromString("^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$");

/* Unsafely unwraps a Js.Nullable */
let unwrap_nullable = (n: Js.Nullable.t('a)): 'a => {
  switch (n |> Js.Nullable.toOption) {
  | Some(a) => a
  | _ => raise(Not_found)
  };
};

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
  switch (Js.Re.exec(s, re2)) {
  | Some(r) =>
    let captures = Js.Re.captures(r) |> Array.map(unwrap_nullable);
    {
      id: int_of_string(captures[1]),
      x: int_of_string(captures[2]),
      y: int_of_string(captures[3]),
      width: int_of_string(captures[4]),
      height: int_of_string(captures[5]),
    };
  | _ => raise(Not_found)
  };
};

let claims = lines |> List.map(string_to_claim);

type square = (int, int);

module SquareMap =
  Map.Make({
    type t = square;
    let compare = compare;
  });

/* Return a generator that yields values from [start, start+len) */
let n_iter = (start: int, len: int): gen(option(int)) => {
  let cur = ref(0);
  () =>
    if (cur^ >= len) {
      None;
    } else {
      cur := cur^ + 1;
      Some(start + cur^ - 1);
    };
};

/* Given a claim return a list of squares it covers */
let claim_to_squares = (c: claim): list(square) => {
  let xs = n_iter(c.x, c.width) |> gen_to_list;
  let ys = n_iter(c.y, c.height) |> gen_to_list;
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

Js.log(overlapping_squares);

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
Js.log(nonoverlapping_claims);