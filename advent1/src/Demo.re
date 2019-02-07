// TODO(advait): I wish I could read from stdin...
let text = Node_fs.readFileSync("test.txt", `utf8);

let re = Js.Re.fromStringWithFlags("(^.+$)", ~flags="gm");

let rec gen_lines = () => {
  let line =
    switch (Js.Re.exec(text, re)) {
    | Some(r) => Js.Re.captures(r)[0] |> Js.Nullable.toOption
    | _ => None
    };
  switch (line) {
  | Some(s) => [s, ...gen_lines()]
  | _ => []
  };
};

let lines = gen_lines();
let ints = List.map(int_of_string, lines);
let sum = List.fold_left((a, b) => a + b, 0, ints);

// Generator
type gen('a) = unit => 'a;

// Given a list l, return a generator that will cycle through l
let infinite_iter = (l: list(int)): gen(int) => {
  let original_l = l;
  let current = ref(l);
  () => {
    if (current^ == []) {
      current := original_l;
    };
    switch (current^) {
    | [head, ...tail] =>
      current := tail;
      head;
    | _ => 0
    };
  };
};

let infinite_ints = infinite_iter(ints);

module OrderedInt = {
  type t = int;
  let compare: (t, t) => int =
    (a, b) => {
      let diff = a - b;
      if (diff > 0) {
        1;
      } else if (diff < 0) {
        (-1);
      } else {
        0;
      };
    };
};

module IntSet = Set.Make(OrderedInt);

let rec repeats = (cur: int, visited: IntSet.t, l: gen(int)): int =>
  if (IntSet.mem(cur, visited)) {
    cur;
  } else {
    let visited_ = IntSet.add(cur, visited);
    repeats(cur + l(), visited_, l);
  };

Js.log(repeats(0, IntSet.empty, infinite_ints));