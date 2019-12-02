let nPlayers = 411;
let maxMarble = 72059;

type marble = int;

type circle = list(marble);
type game = {
  circle,
  pos: int,
};

/* Returns the marble at the current position */
let getCurrent = (game): marble => {
  assert(game.pos < game.circle->Belt_List.length);
  game.circle->Belt_List.getExn(game.pos);
};

let cleanIndex = (circle, at: int): int => {
  switch (at mod circle->Belt_List.length) {
  | n when n < 0 => n + circle->Belt_List.length
  | n => n
  };
};

/* Insert the given marble at the current position */
let insert = (game, marble): game => {
  assert(game.pos < game.circle->Belt_List.length);
  let head = game.circle->Belt_List.take(game.pos)->Belt_Option.getExn;
  let tail = game.circle->Belt_List.drop(game.pos)->Belt_Option.getExn;
  let circle = Belt_List.concat(head, [marble, ...tail]);
  {circle, pos: game.pos};
};

let get = (circle, at) => {
  let realIndex = at mod circle->Belt_List.length;
  circle->Belt_List.get(realIndex)->Belt_Option.getExn;
};

/* Remove the marble at the current position, returning it. */
let remove = (game: game): (game, marble) => {
  assert(game.pos < game.circle->Belt_List.length);
  let head = game.circle->Belt_List.take(game.pos)->Belt_Option.getExn;
  let tail = game.circle->Belt_List.drop(game.pos)->Belt_Option.getExn;
  let toDrop = tail->Belt_List.headExn;
  let newTail = tail->Belt_List.drop(1)->Belt_Option.getExn;
  let circle = Belt_List.concat(head, newTail);
  ({circle, pos: cleanIndex(circle, game.pos)}, toDrop);
};

let shiftPos = (game, by: int): game => {
  {circle: game.circle, pos: cleanIndex(game.circle, game.pos + by)};
};

let max = (l: list(int)): int => {
  l->Belt_List.reduce(0, (acc, score) => max(acc, score));
};

let assertEqual = (a: list('a), b: list('a)) =>
  if (a != b) {
    Js.log3("Expected equality", a->Belt_List.toArray, b->Belt_List.toArray);
  };

module IntMap = Belt_Map.Int;
type scores = IntMap.t(int);
type world = {
  game,
  scores,
  nextMarble: marble,
};

let player = world => world.nextMarble mod nPlayers;

let addScore = (scores, player, amount) => {
  let newScore = scores->IntMap.getWithDefault(player, 0) + amount;
  scores->IntMap.set(player, newScore);
};

let rec play = (world): world =>
  if (world.nextMarble > maxMarble) {
    world;
  } else if (world.nextMarble != 0 && world.nextMarble mod 23 == 0) {
    let scores = world.scores->addScore(world->player, world.nextMarble);
    let game = shiftPos(world.game, -7);
    let (game, removedMarble) = remove(game);
    let scores = scores->addScore(world->player, removedMarble);
    play({game, scores, nextMarble: world.nextMarble + 1});
  } else {
    let game = shiftPos(world.game, 2);
    let game = game->insert(world.nextMarble);
    play({game, scores: world.scores, nextMarble: world.nextMarble + 1});
  };

let game = {circle: [0], pos: 0};
let world = play({game, scores: IntMap.empty, nextMarble: 1});

let maxScore = world.scores->IntMap.valuesToArray->Belt_List.fromArray->max;
Js.log(maxScore);
