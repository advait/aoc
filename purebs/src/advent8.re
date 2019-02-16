let text = Node.Fs.readFileAsUtf8Sync("../inputs/8.txt");
let lines =
  text |> Js.String.trim |> Js.String.split(" ") |> Belt.List.fromArray;
type numbers = list(int);
let numbers: numbers = lines->Belt_List.map(int_of_string);

/* Return a list of ints from start up to and excluding end_ */
let rec range = (start: int, end_: int) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...range(start + 1, end_)];
  };

/* Iterate over l, passing each item as well as an accumulator to f,
   requiring f to produce a mapped item as well as the next accumulator */
let rec mapReduce =
        (l: list('a), acc: 'b, f: ('a, 'b) => ('c, 'b)): (list('c), 'b) => {
  switch (l) {
  | [] => ([], acc)
  | [a, ...tail] =>
    let (b, acc) = f(a, acc);
    let (mappedTail, acc) = mapReduce(tail, acc, f);
    ([b, ...mappedTail], acc);
  };
};

let rec sum = (l: list(int)): int => {
  switch (l) {
  | [] => 0
  | [head, ...tail] => head + sum(tail)
  };
};

type node = {
  children: list(node),
  metadata: list(int),
};

/* Recursively parse the input returning a node and the remaining numbers */
let rec parse = (input: numbers): (node, numbers) => {
  assert(input->Belt_List.length >= 2);
  switch (input) {
  | [nChildren, nMetadata, ...remainingInput] =>
    let (children, remainingInput) =
      range(0, nChildren)
      ->mapReduce(remainingInput, (_, input) => parse(input));
    let (metadata, remainingInput) =
      remainingInput->Belt_List.splitAt(nMetadata)->Belt_Option.getExn;
    let node = {children, metadata};
    (node, remainingInput);
  | _ => Js.Exn.raiseError("Failed to parse")
  };
};

let rec sumMetadata = (n: node): int => {
  let mySum = n.metadata->sum;
  let childSum = n.children->Belt_List.map(sumMetadata)->sum;
  mySum + childSum;
};

let (root, remaining) = parse(numbers);
assert(remaining == []);
Js.log(sumMetadata(root));