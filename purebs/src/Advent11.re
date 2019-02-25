open Util;

let gridSerialNumber = 5093;
let gridWidth = 300;

type cell = (int, int);

let grid = (width, height) => {
  rangeInclusive(1, width)
  ->Belt_List.map(x => rangeInclusive(1, height)->Belt_List.map(y => (x, y)))
  ->Belt_List.flatten;
};

let powerOfCell = ((x, y), gridSerialNumber) => {
  let rackID = x + 10;
  let power = rackID * y;
  let power = power + gridSerialNumber;
  let power = power * rackID;
  let power = power / 100 mod 10;
  power - 5;
};

module CellComparable =
  Belt.Id.MakeComparable({
    type t = cell;
    let cmp = ((a0, a1), (b0, b1)) =>
      switch (Pervasives.compare(a0, b0)) {
      | 0 => Pervasives.compare(a1, b1)
      | c => c
      };
  });

let cellPowers =
  grid(gridWidth, gridWidth)
  ->Belt_List.map(c => {
      let power = powerOfCell(c, gridSerialNumber);
      (c, power);
    })
  ->Belt_List.toArray
  ->Belt_Map.fromArray(~id=(module CellComparable));

let squarePowers =
  grid(gridWidth - 3, gridWidth - 3)
  ->Belt_List.map(c => {
      let (x, y) = c;
      let c1 = (x, y);
      let c2 = (x + 1, y);
      let c3 = (x + 2, y);
      let c4 = (x, y + 1);
      let c5 = (x + 1, y + 1);
      let c6 = (x + 2, y + 1);
      let c7 = (x, y + 2);
      let c8 = (x + 1, y + 2);
      let c9 = (x + 2, y + 2);
      let cells = [c1, c2, c3, c4, c5, c6, c7, c8, c9];
      let squarePower = cells->Belt_List.map(c => cellPowers->Belt_Map.getExn(c))->sumList;
      (c, squarePower);
    })
  ->Belt_List.toArray
  ->Belt_Map.fromArray(~id=(module CellComparable));

let (biggestSquare, biggestPower) =
  squarePowers->Belt_Map.reduce(
    ((0, 0), (-99)),
    (acc, square, power) => {
      let (curSquare, curPower) = acc;
      if (curPower > power) {
        (curSquare, curPower);
      } else {
        (square, power);
      };
    },
  );

Js.log2(biggestSquare, biggestPower);
