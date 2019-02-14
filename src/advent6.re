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

let assert_equal_set = (a: BatSet.t('a), b: BatSet.t('a)): unit => {
  if (!BatSet.equal(a, b)) {
    print_endline("Failed assert:\na:" ++ dump(a) ++ "\nb:" ++ dump(b));
    let diff = BatSet.sym_diff(a, b);
    print_endline("A only: " ++ dump(BatSet.intersect(a, diff)));
    print_endline("B only: " ++ dump(BatSet.intersect(b, diff)));
  };
  assert(BatSet.equal(a, b));
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

/* Diagonal directions */
type dir =
  | DR
  | DL
  | UL
  | UR;

/* Given a point, take a step in direction d */
let step = (d: dir, p: point): point => {
  switch (d) {
  | DR => {x: p.x + 1, y: p.y + 1}
  | DL => {x: p.x - 1, y: p.y + 1}
  | UL => {x: p.x - 1, y: p.y - 1}
  | UR => {x: p.x + 1, y: p.y - 1}
  };
};
let step_test = () => {
  assert_equal(step(DR, point(0, 0)), point(1, 1));
  assert_equal(step(DR, point(0, -1)), point(1, 0));
};
let _ = step_test();

/* Given a trace direction, return the next direction to follow to
   trace a diamond clockwise */
let next_dir = (d: dir): option(dir) => {
  switch (d) {
  | DR => Some(DL)
  | DL => Some(UL)
  | UL => Some(UR)
  | UR => None
  };
};

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

/* Return all the points around p of distance exactly=n */
let points_around = (n: int, p: point): BatSet.t(point) => {
  let b: bounding_box = {x0: p.x - n, x1: p.x + n, y0: p.y - n, y1: p.y + n};
  let rec temp = (cur: point, d: dir): BatSet.t(point) => {
    let next = step(d, cur);
    if (contains(next, b)) {
      let next_set = temp(next, d);
      BatSet.add(cur, next_set);
    } else {
      switch (next_dir(d)) {
      | Some(d) => temp(cur, d)
      | None => BatSet.empty
      };
    };
  };
  let top_point = {x: p.x, y: p.y - n};
  /*let starting_point = step(DR, top_point);*/
  temp(top_point, DR) |> comb(ret => assert(ret |> BatSet.cardinal == 4 * n));
};

/* Return all the points around p of distance <= n */
let all_points_around = (n: int, p: point): BatSet.t(point) => {
  let rec temp = (n: int): BatSet.t(point) => {
    switch (n) {
    | 0 => BatSet.of_list([p])
    | n =>
      let a = points_around(n, p);
      let b = temp(n - 1);
      BatSet.union(a, b) |> comb(_ => print_endline("d" ++ dump(n)));
    };
  };
  temp(n) |> comb(_ => print_endline("Creating set"));
};

let all_points_around_test = () => {
  assert_equal_set(
    all_points_around(0, point(0, 0)),
    [point(0, 0)] |> BatSet.of_list,
  );
  assert_equal_set(
    all_points_around(1, point(0, 0)),
    [point(0, -1), point(1, 0), point(0, 1), point(-1, 0), point(0, 0)]
    |> BatSet.of_list,
  );
  assert_equal_set(
    all_points_around(2, point(0, 0)),
    [
      point(0, -2),
      point(1, -1),
      point(2, 0),
      point(1, 1),
      point(0, 2),
      point(-1, 1),
      point(-2, 0),
      point(-1, -1),
      point(0, -1),
      point(1, 0),
      point(0, 1),
      point(-1, 0),
      point(0, 0),
    ]
    |> BatSet.of_list,
  );
  /*
   assert_equal(
     all_points_around(4000, point(0, 0)) |> BatSet.cardinal,
     32008001,
   );
   */
};
let _ = all_points_around_test();

let max_distance = 10000 - 1;

let nmost_point = (l: list(point), comp: (point, point) => bool): point => {
  l
  |> BatList.reduce((p1, p2) =>
       if (comp(p1, p2)) {
         p1;
       } else {
         p2;
       }
     );
};
let leftmost = (p1: point, p2: point) => p1.x < p2.x;
let rightmost = (p1: point, p2: point) => p1.x >= p2.x;
let topmost = (p1: point, p2: point) => p1.y < p2.y;
let bottommost = (p1: point, p2: point) => p1.y >= p2.y;

let leftmost_point = nmost_point(points, leftmost);
let topmost_point = nmost_point(points, topmost);
let rightmost_point = nmost_point(points, rightmost);
let bottommost_point = nmost_point(points, bottommost);

let key_points = [
  leftmost_point,
  topmost_point,
  rightmost_point,
  bottommost_point,
];
let all_sets =
  key_points |> BatList.map(p => all_points_around(max_distance, p));
let intersection =
  all_sets |> BatList.reduce((s1, s2) => BatSet.intersect(s1, s2));
print_endline(
  "Intersection size: " ++ string_of_int(BatSet.cardinal(intersection)),
);