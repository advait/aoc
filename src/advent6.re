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

let lines = File.lines_of("../test.txt") |> BatList.of_enum;

type point = {
  x: int,
  y: int,
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
  |> BatEnum.map(x =>
       BatEnum.(minY -- maxY) |> BatEnum.map(y => {x, y}) |> BatList.of_enum
     )
  |> BatList.of_enum
  |> BatList.flatten;

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

print_endline(dump(biggest_area));