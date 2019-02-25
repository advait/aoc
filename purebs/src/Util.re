/* Unwraps the given nullable, throwing if it is null */
let getNullableExn = (v: Js.nullable('a)): 'a => {
  v |> Js.Nullable.toOption |> Belt.Option.getExn;
};

/* Returns a list of captures for the given string or [] if there are none */
let reMatch = (s: string, re: Js.Re.t): list(string) => {
  switch (Js.Re.exec(s, re)) {
  | Some(r) =>
    let cap = Js.Re.captures(r)->Belt_Array.map(getNullableExn);
    cap->Belt_List.fromArray->Belt_List.tail->Belt_Option.getExn;
  | None => []
  };
};

let optionGetExn = o => {
  switch (o) {
  | Some(v) => v
  | None => Js.Exn.raiseRangeError("Could not unwrap Option None")
  };
};

let rec rangeInclusive = (start, end_) =>
  if (start > end_) {
    [];
  } else {
    [start, ...rangeInclusive(start + 1, end_)];
  };

let rec iterInclusiveFloat = (start, end_, f) =>
  if (start > end_) {
    ();
  } else {
    f(start);
    iterInclusiveFloat(start +. 1., end_, f);
  };

/* Sum the items in list l */
let rec sumList = l => {
  switch (l) {
  | [] => 0
  | [head, ...tail] => head + sumList(tail)
  };
};
