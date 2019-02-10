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

/* Returns the (k, v) pair for the maximum value in the map */
let rec max_map_value = (m: BatMap.t('a, 'b)): ('a, 'b) => {
  switch (m) {
  | m when m == BatMap.empty => ((-1), (-1))
  | m =>
    let ((k1, v1), m_) = BatMap.pop(m);
    let (k2, v2) = max_map_value(m_);
    if (max(v1, v2) == v1) {
      (k1, v1);
    } else {
      (k2, v2);
    };
  };
};

let lines = File.lines_of("../test.txt") |> BatList.of_enum;

type datetime = {
  year: float,
  month: float,
  date: float,
  hours: float,
  minutes: float,
};

/* Return the number of minutes between a and b */
let datetime_diff = (a: datetime, b: datetime): float => {
  assert(a.year == b.year);
  assert(a.month == b.month);
  assert(a.date == b.date);
  assert(a.hours == 0.);
  assert(b.hours == 0.);
  b.minutes -. a.minutes -. (b.hours -. a.hours) *. 60.;
};

/* Just a timestamp and an opaque event string */
type rawEvent = {
  ts: datetime,
  e: string,
};

/*
 compare_all chains together multiple compare functions, falling back to later ones
 if earlier ones return zero.
 */
let rec compare_all = (l: list(('a, 'a) => int), a: 'a, b: 'a): int => {
  switch (l) {
  | [] => 0
  | [c, ...tail] =>
    switch (c(a, b)) {
    | 0 => compare_all(tail, a, b)
    | other => other
    }
  };
};

/*
 let compare = (a: rawEvent, b: rawEvent): int => compare(a, b);
 */
let compare = (a: rawEvent, b: rawEvent): int => {
  compare_all(
    [
      (a, b) => compare(a.year, b.year),
      (a, b) => compare(a.month, b.month),
      (a, b) => compare(a.date, b.date),
      (a, b) => compare(a.hours, b.hours),
      (a, b) => compare(a.minutes, b.minutes),
    ],
    a.ts,
    b.ts,
  );
};

/* [1518-11-01 00:00] Guard #10 begins shift */
let re2 = Re.Perl.compile_pat({|^\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.*)$|});

let lineToRawEvent = (s: string): rawEvent => {
  let groups = Re.Pcre.extract(~rex=re2, s);
  let ts = {
    year: float_of_string(groups[1]),
    month: float_of_string(groups[2]),
    date: float_of_string(groups[3]),
    hours: float_of_string(groups[4]),
    minutes: float_of_string(groups[5]),
  };
  {ts, e: groups[6]};
};

/* Sorted raw events */
let rawEvents = lines |> List.map(lineToRawEvent) |> List.sort(compare);

type happening =
  | BeginsShift
  | FallsAsleep
  | WakesUp;

type guard = int;

type event = {
  ts: datetime,
  g: guard,
  h: happening,
};

let re3 = Re.Perl.compile_pat({|^Guard #(\d+).*$|});

let rawEventToEvent = (e: rawEvent, currentGuard: guard): event => {
  switch (e.e) {
  | "falls asleep" => {ts: e.ts, g: currentGuard, h: FallsAsleep}
  | "wakes up" => {ts: e.ts, g: currentGuard, h: WakesUp}
  | s =>
    let groups = Re.Pcre.extract(~rex=re3, s);
    let newGuard: guard = int_of_string(groups[1]);
    {ts: e.ts, g: newGuard, h: BeginsShift};
  };
};

let rawEventsToEvents = (l: list(rawEvent)): list(event) => {
  let rec temp =
          (currentGuard: guard, l: list(rawEvent), acc: list(event))
          : list(event) => {
    switch (l) {
    | [] => acc
    | [head, ...tail] =>
      let e = rawEventToEvent(head, currentGuard);
      temp(e.g, tail, [e, ...acc]);
    };
  };
  temp(-1, l, []) |> List.rev;
};

let events = rawEvents |> rawEventsToEvents;

type guardMap = BatMap.t(int, list(event));

let eventsByGuard: guardMap =
  events
  |> BatList.fold_left(
       (acc, e) => {
         let l = acc |> BatMap.find_default([], e.g);
         /* Add events in reverse */
         acc |> BatMap.add(e.g, [e, ...l]);
       },
       BatMap.empty,
     )
  |> BatMap.map(l => l |> BatList.rev); /* Reverse events */

/* let eventsByGuard2 = events |> group_by(e => e.g); */

/* time_asleep returns the total time that a guard was asleep given a list of events. */
let rec time_asleep = (l: list(event)): int => {
  switch (l) {
  | [] => 0
  | [a, b, ...tail] when a.h == FallsAsleep =>
    assert(b.h == WakesUp || b.h == BeginsShift);
    assert(a.g == b.g);
    let this_sleep = int_of_float(datetime_diff(a.ts, b.ts));
    assert(this_sleep > 0);
    this_sleep + time_asleep(tail);
  | [_, ...tail] => time_asleep(tail)
  };
};

let rec mins_asleep = (l: list(event)): list(int) => {
  switch (l) {
  | [] => []
  | [a, b, ...tail] when a.h == FallsAsleep =>
    assert(b.h == WakesUp || b.h == BeginsShift);
    assert(a.g == b.g);
    assert(a.ts.hours == 0.);
    assert(b.ts.hours == 0.);
    let mins =
      BatEnum.(
        int_of_float(a.ts.minutes) -- (int_of_float(b.ts.minutes) - 1)
      )
      |> BatList.of_enum;
    BatList.append(mins, mins_asleep(tail));
  | [_, ...tail] => mins_asleep(tail)
  };
};

let sleep_by_guard = eventsByGuard |> BatMap.map(l => l |> time_asleep);

let mins_asleep_by_guard =
  eventsByGuard
  |> BatMap.map(mins_asleep)
  |> BatMap.map(mins =>
       mins |> group_by(identity) |> BatMap.map(BatList.length)
     );

let (g, sleep) = sleep_by_guard |> max_map_value;
print_endline("Guard: " ++ string_of_int(g));
print_endline("Sleep: " ++ string_of_int(sleep));

let (max_min, appearances) =
  mins_asleep_by_guard |> BatMap.find(g) |> max_map_value;
print_endline("Max min: " ++ string_of_int(max_min));
print_endline("Appearancesl: " ++ string_of_int(appearances));

print_endline("Answer #1: " ++ string_of_int(g * max_min));