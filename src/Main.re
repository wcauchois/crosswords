let canvas = Dom.getById("c");

let context = Canvas.getContext(canvas);

module Ctx = Canvas.Ctx;

let across = Util.Horizontal;

let down = Util.Vertical;

let clues: list(Board.clue) = [
  {x: 3, y: 0, t: "One with pointy-toed shoes", o: across, a: ""}
];

let blocks = [(0, 0), (1, 0), (2, 0), (6, 0), (7, 0), (8, 0), (0, 1), (8, 1)];

let board =
  Board.empty(9, 9, clues)
  |> (
    (b: Board.t) =>
      List.fold_left(
        (b, (x, y)) => Board.setState(x, y, Board.Blocked, b),
        b,
        blocks
      )
  );

/*let board =
    Board.empty(10, 10, clues)
    |> Board.setState(0, 0, Board.Blocked)
    |> Board.setState(4, 0, Board.Blocked)
    |> Board.setState(5, 0, Board.Blocked)
    |> Board.setState(5, 5, Board.Blocked)
    |> Board.setState(5, 6, Board.Blocked)
    |> Board.setState(5, 7, Board.Blocked)
    |> (
      (b: Board.t) =>
        List.fold_left(
          (b: Board.t, (i, c): (int, char)) =>
            Board.setState(1 + i, 2, Board.Full(c), b),
          b,
          List.mapi((i, c) => (i, c), Util.explodeString("hello"))
        )
    );
  */
Board.draw(board, context);

module Observable = Bacon.Observable;

module KeyboardEvent = Dom.KeyboardEvent;

type keyboardInput =
  | Left
  | Right
  | Up
  | Down
  | SpaceBar;

let keyObs: Bacon.observable(keyboardInput) =
  Bacon.capturingKeyboardObservable(event =>
    switch (KeyboardEvent.key(event)) {
    | "ArrowUp" => Some(Up)
    | "ArrowDown" => Some(Down)
    | "ArrowLeft" => Some(Left)
    | "ArrowRight" => Some(Right)
    | " " => Some(SpaceBar)
    | _ => None
    }
  );

let stateObs =
  Observable.scan(
    keyObs,
    BoardState.empty(board),
    (s, key) => {
      let (oldX, oldY) = s.cursor;
      let newCursor =
        switch key {
        | Up => (oldX + 0, oldY - 1)
        | Down => (oldX + 0, oldY + 1)
        | Left => (oldX - 1, oldY + 0)
        | Right => (oldX + 1, oldY + 0)
        | _ => (oldX, oldY)
        };
      let newOrientation =
        if (key == SpaceBar) {
          BoardState.flipOrientation(s.orientation);
        } else {
          s.orientation;
        };
      {cursor: newCursor, orientation: newOrientation};
    }
  );

let initBoardState = BoardState.empty(board);

let boardObs =
  Observable.map(stateObs, state => BoardState.applyModifiers(board, state));

Observable.onValue(
  boardObs,
  b => {
    Canvas.Ctx.clearRect(context, 0.0, 0.0, 480.0, 480.0);
    Board.draw(b, context);
  }
);