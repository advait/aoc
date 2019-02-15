let text = Node.Fs.readFileAsUtf8Sync("../inputs/7.txt");
let lines =
  text |> Js.String.trim |> Js.String.split("\n") |> Belt.List.fromArray;

/* Step C must be finished before step A can begin. */
let re =
  Js.Re.fromString(
    {|^Step ([A-Z]) must be finished before step ([A-Z]) can begin.$|},
  );

let unwrap = (v: Js.nullable('a)): 'a => {
  v |> Js.Nullable.toOption |> Belt.Option.getExn;
};

let re_match = (s: string, re: Js.Re.t): list(string) => {
  switch (Js.Re.exec(s, re)) {
  | Some(r) =>
    let cap = Js.Re.captures(r)->Belt_Array.map(unwrap);
    cap->Belt_List.fromArray->Belt_List.tail->Belt_Option.getExn;
  | None => []
  };
};

type dep = (string, string);
let depList =
  lines->Belt.List.map(s =>
    switch (re_match(s, re)) {
    | [from, to_] => (from, to_)
    | _ => Js.Exn.raiseError("Invalid input")
    }
  );

module StringMap = Belt_Map.String;
type depMap = StringMap.t(list(string));

let allDeps: depMap =
  depList->Belt_List.reduce(
    StringMap.empty,
    (acc, entry) => {
      let (dep, target) = entry;
      let deps = acc->StringMap.getWithDefault(target, []);
      let acc = acc->StringMap.set(target, [dep, ...deps]);
      if (!acc->StringMap.has(dep)) {
        /* We have brand new node with no dependencies */
        acc->StringMap.set(dep, []);
      } else {
        acc;
      };
    },
  );

type depEntry = (string, list(string));
let compareDepEntries = (e1: depEntry, e2: depEntry): int => {
  let (node1, deps1) = e1;
  let (node2, deps2) = e2;
  switch (compare(deps1->Belt_List.length, deps2->Belt_List.length)) {
  | 0 => compare(node1, node2) /* Fall back to alphabetical node compare */
  | n => n
  };
};

let rec visitOrder = (deps: depMap): list(string) =>
  if (deps->StringMap.isEmpty) {
    [];
  } else {
    /* Determine which node is next to free up */
    let (nextNode, nextDeps) =
      deps
      ->StringMap.toList
      ->Belt_List.sort(compareDepEntries)
      ->Belt_List.headExn;
    assert(nextDeps == []);

    /* Remove node netry */
    let deps = deps->StringMap.remove(nextNode);

    /* Remove anywhere where this node shows up in deps */
    let deps = deps->StringMap.map(l => l |> List.filter(n => n != nextNode));
    [nextNode, ...visitOrder(deps)];
  };

Js.log(Js.Array.joinWith("", visitOrder(allDeps)->Belt_List.toArray));