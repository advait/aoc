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

module OrderedChar = {
  type t = char;
  let compare: (t, t) => int =
    (a, b) => {
      let diff = Char.code(a) - Char.code(b);
      if (diff > 0) {
        1;
      } else if (diff < 0) {
        (-1);
      } else {
        0;
      };
    };
};

module CharMap = Map.Make(OrderedChar);

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

let counts =
  map_gen(
    o =>
      switch (o) {
      | None => None
      | Some(s) => Some(get_char_map(s))
      },
    lines |> list_to_gen,
  );

let (twos, threes) =
  fold_gen((acc, m) => tp(acc, has_two_three(m)), (0, 0), counts);

Js.log(twos);
Js.log(threes);
Js.log(twos * threes);

/* Given two box IDs return the number of chars that differ from each */
let rec comp_boxes = (a: string, b: string): int =>
  if (a == "" || b == "") {
    0;
  } else {
    let a_ = String.sub(a, 1, String.length(a) - 1);
    let b_ = String.sub(b, 1, String.length(b) - 1);
    let remaining = comp_boxes(a_, b_);
    a.[0] == b.[0] ? remaining : 1 + remaining;
  };

let sorted_lines = List.sort(String.compare, lines);

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

Js.log(sorted_lines |> one_comp);