/* TODO(advait): I wish I could read from stdin... */
let text = Node_fs.readFileSync("test.txt", `utf8);

let re = Js.Re.fromStringWithFlags("(^.+$)", ~flags="gm");

/* Generator */
type gen('a) = unit => 'a;

let map_gen = (f: 'a => 'b, g: gen('a)): gen('b) => {
  () => f(g());
};

let gen_lines = (): gen(option(string)) => {
  () => {
    switch (Js.Re.exec(text, re)) {
    | Some(r) => Js.Re.captures(r)[0] |> Js.Nullable.toOption
    | _ => None
    };
  };
};

let lines = gen_lines();
let ints =
  map_gen(
    o =>
      switch (o) {
      | Some(s) => Some(int_of_string(s))
      | None => None
      },
    lines,
  );

let sum_gen = (g: gen(option(int))): int => {
  let rec acc = i => {
    switch (g()) {
    | Some(j) => acc(i + j)
    | None => i
    };
  };
  acc(0);
};

Js.log(sum_gen(ints));

let gen_to_list = (g: gen(option('a))): list('a) => {
  let rec aux = (acc): list('a) => {
    switch (g()) {
    | None => acc
    | Some(a) => aux([a, ...acc])
    };
  };
  List.rev(aux([]));
};

/* Given a list l, return a generator that will cycle through l */
let infinite_iter = (g: gen(option('a))): gen('a) => {
  let original_l = gen_to_list(g);
  let l = ref(original_l);
  () => {
    if (l^ == []) {
      l := original_l;
    };
    switch (l^) {
    | [head, ...tail] =>
      l := tail;
      head;
    | [] => raise(Not_found)
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