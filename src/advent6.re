open Batteries;

/* Given a function which_groups, returns a map of the elements of l grouped by the function. */
let group_by = (which_group: 'a => 'g, l: list('a)): BatMap.t('g, list('a)) => {
  l
  |> BatList.fold_left(
       (acc: BatMap.t('g, list('a)), i: 'a): BatMap.t('g, list('a)) => {
         let group = which_group(i);
         let items = acc |> BatMap.find_default([], group);
         acc |> BatMap.add(group, BatList.append(items, [i]));
       },
       BatMap.empty,
     );
};

let comb = (f: 'a => unit, x: 'a): 'a => {
  f(x);
  x;
};

let assert_equal = (a: 'a, b: 'a): unit => {
  if (a != b) {
    print_endline("Failed assert:\na:" ++ dump(a) ++ "\nb:" ++ dump(b));
  };
  assert(a == b);
};

let lines = File.lines_of("../inputs/6.txt") |> BatList.of_enum;

type point = {
  x: int,
  y: int,
};

let point = (x: int, y: int): point => {
  {x, y};
};

let re2 = Re.Perl.compile_pat({|^(\d+), (\d+)$|});
let point_of_string = (s: string): point => {
  let groups = Re.Pcre.extract(~rex=re2, s);
  {x: int_of_string(groups[1]), y: int_of_string(groups[2])};
};

let points = lines |> BatList.map(point_of_string);
let minX = points |> BatList.map(p => p.x) |> BatList.min;
let maxX = points |> BatList.map(p => p.x) |> BatList.max;
let minY = points |> BatList.map(p => p.y) |> BatList.min;
let maxY = points |> BatList.map(p => p.y) |> BatList.max;

let field: list(point) =
  BatEnum.(minX -- maxX)
  |> BatEnum.map(x => BatEnum.(minY -- maxY) |> BatEnum.map(y => {x, y}))
  |> BatEnum.flatten
  |> BatList.of_enum;

/* Manhattan distance between two points */
let len = (a: point, b: point) => abs(a.x - b.x) + abs(a.y - b.y);

/* Given anchor point f and test points l, return a list of points
   closest to f along with the distance */
let closest_points = (f: point, l: list(point)): (int, list(point)) => {
  l
  |> BatList.fold_left(
       (acc, p) => {
         let (cur_closest, cur_points) = acc;
         let distance = len(f, p);
         if (distance < cur_closest) {
           (distance, [p]);
         } else if (distance == cur_closest) {
           (distance, [p, ...cur_points]);
         } else {
           (cur_closest, cur_points);
         };
       },
       (999999999, []),
     );
};

let field_of_closest_points =
  field |> BatList.map(f => closest_points(f, points));
let biggest_area =
  field_of_closest_points
  |> BatList.filter(s => {
       /* Remove field points where there are multiple closest */
       let (_, points) = s;
       points |> BatList.length == 1;
     })
  |> BatList.map(s => {
       /* Select the point coords */
       let (_, points) = s;
       points |> BatList.hd;
     })
  |> group_by(identity)
  |> BatMap.map(BatList.length)  /* Count number of point occurrances */
  |> BatMap.values
  |> BatList.of_enum
  |> BatList.max;

/* Part one answer */
print_endline("Part 1: " ++ string_of_int(biggest_area));

/* Begin part two */

/* Represents a square bounding box */
type bounding_box = {
  x0: int,
  x1: int,
  y0: int,
  y1: int,
};

/* Returns whether the point is contained in the box */
let contains = (p: point, b: bounding_box): bool => {
  p.x >= b.x0 && p.x <= b.x1 && p.y >= b.y0 && p.y <= b.y1;
};

/* Return a bounding box whose edge encapsulates b */
let bigger_box = (b: bounding_box) => {
  x0: b.x0 - 1,
  x1: b.x1 + 1,
  y0: b.y0 - 1,
  y1: b.y1 + 1,
};

type dir =
  | Right
  | Down
  | Left
  | Up;

/* Return the next direction to go to trace a clockwise path. */
let next_dir = (d: dir): dir => {
  switch (d) {
  | Right => Down
  | Down => Left
  | Left => Up
  | Up => Right
  };
};

/* Return a new point that is a step in d from p */
let step = (d: dir, p: point): point => {
  switch (d) {
  | Right => {x: p.x + 1, y: p.y}
  | Down => {x: p.x, y: p.y + 1}
  | Left => {x: p.x - 1, y: p.y}
  | Up => {x: p.x, y: p.y - 1}
  };
};

/* Return a list of points that denote the edge of the bounding box */
let iter_edge = (b: bounding_box): list(point) =>
  if (b.x0 == b.x1 && b.y0 == b.y1) {
    [
      /* Special case for origin box */
      {x: b.x0, y: b.y0},
    ];
  } else {
    let top_left: point = {x: b.x0, y: b.y0};
    let rec temp = (cur: point, d: dir): list(point) =>
      if (cur == top_left && d == Up) {
        [];
          /* We've made an entire clockwise wrap around the box */
      } else {
        let next = step(d, cur);
        if (!contains(next, b)) {
          /* Stepping in this direction leaves the box. Change dir. */
          temp(
            cur,
            next_dir(d),
          );
        } else {
          [cur, ...temp(next, d)];
        };
      };
    temp(top_left, Right);
  };

/* Returns whether all points in l are < n distance of p. */
let all_points_distance = (p: point, l: list(point)): int => {
  l |> BatList.map(p_ => len(p, p_)) |> BatList.sum;
};

let max_distance = 10000;

/* Recursively expands boxes around the orgin until the outer rim contains no hits */
let count_points_within = (n: int, l: list(point)): int => {
  let rec temp = (b: bounding_box): int => {
    let count =
      b
      |> iter_edge
      |> BatList.filter(p => all_points_distance(p, l) < n)
      |> BatList.length;
    if (count == 0) {
      0;
    } else {
      count + temp(b |> bigger_box);
    };
  };
  /* let start_point = l |> BatList.hd; */
  let start_point = {x: 200, y: 100};
  temp({
    x0: start_point.x,
    x1: start_point.x,
    y0: start_point.y,
    y1: start_point.y,
  });
};

let c = count_points_within(max_distance, points);
print_endline("Part 2: " ++ string_of_int(c));