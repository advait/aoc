let text = Node.Fs.readFileAsUtf8Sync("../inputs/7.txt");
let lines = text |> Js.String.trim |> Js.String.split("\n") |> Belt.List.fromArray;

/* Step C must be finished before step A can begin. */
let re = Js.Re.fromString({|^Step ([A-Z]) must be finished before step ([A-Z]) can begin.$|});

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

let nodeDeps: depMap =
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

/* Returns a list of nodes with no deps in appropriate order */
let availableNodes = (deps: depMap): list(string) => {
  deps
  ->StringMap.toList
  ->Belt_List.keep(entry => {
      let (_, deps) = entry;
      deps->Belt_List.length == 0;
    })
  ->Belt_List.map(entry => {
      let (node, _) = entry;
      node;
    })
  ->Belt_List.sort(compare);
};

/* Updates the map removing a node and any dep entries for that node */
let removeNode = (deps: depMap, rmNode: string): depMap => {
  /* Remove node netry */
  let deps = deps->StringMap.remove(rmNode);

  /* Remove anywhere where this node shows up in deps */
  deps->StringMap.map(l => l |> List.filter(n => n != rmNode));
};

let rec visitOrder = (deps: depMap): list(string) =>
  if (deps->StringMap.isEmpty) {
    [];
  } else {
    /* Determine which node is next to free up */
    let nextNode = availableNodes(deps)->Belt_List.headExn;
    let deps = removeNode(deps, nextNode);
    [nextNode, ...visitOrder(deps)];
  };

/* Part 1 */
Js.log("Build order: " ++ Js.Array.joinWith("", visitOrder(nodeDeps)->Belt_List.toArray));

/* Part 2 */

/* Return the amount of time needed to assemble the given node */
let nodeTime = (node: string): int => {
  assert(node->String.length == 1);
  let c = node->String.get(0)->int_of_char;
  let ret = c - int_of_char('A') + 1 + 60;
  assert(ret >= 61);
  ret;
};

type timeMap = StringMap.t(int);

let nodeTimes: timeMap =
  nodeDeps->StringMap.keysToArray->Belt_Array.map(node => (node, nodeTime(node)))->StringMap.fromArray;

assert(nodeTimes->StringMap.size == nodeDeps->StringMap.size);
assert(
  Belt_Array.eq(nodeTimes->StringMap.keysToArray, nodeDeps->StringMap.keysToArray, (a, b) => compare(a, b) == 0),
);

let getFinishedNodes = (nodeTimes: timeMap): list(string) => {
  nodeTimes
  ->StringMap.toList
  ->Belt_List.keep(entry => {
      let (_, t) = entry;
      t == 0;
    })
  ->Belt_List.map(entry => {
      let (node, _) = entry;
      node;
    })
  ->Belt_List.sort(compare);
};

type workers = list(string);

let numWorkers = 5;

/* Take up to n elements from list l */
let takeUpTo = (l: list('a), n: int): list('a) =>
  if (l->Belt_List.length <= n) {
    l;
  } else {
    l->Belt_List.take(n)->Belt_Option.getExn;
  };

type world = {
  nodeDeps: depMap,
  nodeTimes: timeMap,
  elapsed: int,
  finished: list(string),
};

assert([]->Belt_List.reduce(1, (_, i) => i) == 1);

let rec tick = (world: world): world => {
  let finishedNodes = world.nodeTimes->getFinishedNodes;
  let nodeTimes = world.nodeTimes->StringMap.removeMany(finishedNodes->Belt_List.toArray);
  let nodeDeps = finishedNodes->Belt_List.reduce(world.nodeDeps, removeNode);

  if (nodeTimes->StringMap.isEmpty || nodeDeps->StringMap.isEmpty) {
    /* Finished with all the nodes! */
    assert(nodeDeps->StringMap.isEmpty);
    assert(nodeTimes->StringMap.isEmpty);
    {nodeDeps, nodeTimes, elapsed: world.elapsed, finished: Belt_List.concat(world.finished, finishedNodes)};
  } else {
    /* How many ticks of work can we perform right now? */
    let workingOn = nodeDeps->availableNodes->takeUpTo(numWorkers);
    let ticksToAdvance =
      workingOn
      ->Belt_List.map(node => nodeTimes->StringMap.getExn(node))
      ->Belt_List.sort(compare)
      ->Belt_List.headExn;

    let nodeTimes =
      nodeTimes->StringMap.mapWithKey((node, timeLeft) =>
        if (workingOn->Belt_List.some(node_ => node_ == node)) {
          timeLeft - ticksToAdvance;
        } else {
          timeLeft;
        }
      );
    tick({nodeDeps, nodeTimes, elapsed: world.elapsed + ticksToAdvance, finished: world.finished});
  };
};

let world = tick({nodeDeps, nodeTimes, elapsed: 0, finished: []});
print_endline("Total ticks: " ++ string_of_int(world.elapsed));
