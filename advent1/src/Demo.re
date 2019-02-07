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

Js.log(sum);