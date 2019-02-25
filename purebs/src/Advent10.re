open Tablecloth;
open Util;

let text = Node.Fs.readFileAsUtf8Sync("../inputs/10.txt");
let lines = text |> Js.String.trim |> Js.String.split("\n") |> Belt_List.fromArray;

type point = {
  x: float,
  y: float,
  dx: float,
  dy: float,
};

/* position=< 9,  1> velocity=< 0,  2>. */
let re = Js.Re.fromString({|^position=<\s*([-0-9]+),\s*([-0-9]+)> velocity=<\s*([-0-9]+),\s*([-0-9]+)>$|});

let pointOfString = s => {
  switch (reMatch(s, re)->List.map(~f=float_of_string)) {
  | [x, y, dx, dy] => {x, y, dx, dy}
  | _ => Js.Exn.raiseError("Bad match")
  };
};

let stepPoint = p => {x: p.x +. p.dx, y: p.y +. p.dy, dx: p.dx, dy: p.dy};

let boundingBox = points => {
  let minX = points->List.map(~f=p => p.x)->List.minimumBy(~f=identity)->optionGetExn;
  let maxX = points->List.map(~f=p => p.x)->List.maximumBy(~f=identity)->optionGetExn;
  let minY = points->List.map(~f=p => p.y)->List.minimumBy(~f=identity)->optionGetExn;
  let maxY = points->List.map(~f=p => p.y)->List.maximumBy(~f=identity)->optionGetExn;
  (minX, maxX, minY, maxY);
};

let boundingBoxArea = (points): float => {
  let (minX, maxX, minY, maxY) = boundingBox(points);
  assert(maxX > minX);
  assert(maxY > minY);
  let area = (maxX -. minX) *. (maxY -. minY);
  assert(area > 0.);
  area;
};

let rec step = (points, n) => {
  let currentArea = points->boundingBoxArea;
  let nextPoints = points->List.map(~f=stepPoint);
  let nextArea = nextPoints->boundingBoxArea;
  if (nextArea > currentArea) {
    (points, n);
  } else {
    step(nextPoints, n + 1);
  };
};

let printPoints = points => {
  let (minX, maxX, minY, maxY) = boundingBox(points);
  rangeInclusiveFloat(
    minY,
    maxY,
    y => {
      rangeInclusiveFloat(minX, maxX, x =>
        if (points |> List.any(~f=p => p.x == x && p.y == y)) {
          print_string("#");
        } else {
          print_string(" ");
        }
      );
      print_newline();
    },
  );
};

let points = lines->List.map(~f=pointOfString);

assert(points->List.getAt(~index=0)->optionGetExn.x == 11153.);

let (points, n) = step(points, 0);
printPoints(points);
Js.log(n);
