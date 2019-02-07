// TODO(advait): I wish I could read from stdin...
let text = Node_fs.readFileSync("test.txt", `utf8);

let re = Js.Re.fromStringWithFlags("(^.+$)", ~flags="gm");

let rec foo = () => {
  switch (Js.Re.exec(text, re)) {
  | Some(r) =>
    switch (Js.Re.captures(r)[0] |> Js.Nullable.toOption) {
    | Some(s) => [s, ...foo()]
    // TODO(advait): I wish I could flatten these Nones...
    | None => []
    }
  | None => []
  };
};
let lines = foo();

let ints = List.map(int_of_string, lines);
let sum = List.fold_left((a, b) => a + b, 0, ints);

Js.log(sum);