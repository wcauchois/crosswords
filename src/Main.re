let canvas = Dom.getById("c");

let context = Canvas.getContext(canvas);

module Ctx = Canvas.Ctx;

let across = Util.Horizontal;

let down = Util.Vertical;

let clues: list(Board.clue) = [
  /* across */
  {x: 1, y: 0, t: "What everyone brings to a potluck dinner", o: across, a: "DISH"},
  {x: 1, y: 1, t: "Swimming lane separator", o: across, a: "ROPE"},
  {x: 0, y: 2, t: "Two-faced Roman god", o: across, a: "JANUS"},
  {x: 0, y: 3, t: "Similar (to)", o: across, a: "AKIN"},
  {x: 0, y: 4, t: "___ Bennett, \"S.N.L.\" cast member who impersonates Putin", o: across, a: "BECK"},
  /* down */
  {x: 0, y: 2, t: "Quick punch", o: down, a: "JAB"},
  {x: 1, y: 0, t: "\"Started From the Bottom\" rapper", o: down, a: "DRAKE"},
  {x: 2, y: 0, t: "Kind of bond in chemistry", o: down, a: "IONIC"},
  {x: 3, y: 0, t: "Courageous spirit", o: down, a: "SPUNK"},
  {x: 4, y: 0, t: "___ Just Not That Into You", o: down, a: "HES"},
];

let blocks = [(0, 0), (0, 1), (4, 3), (4, 4)];

let board =
  Board.empty(5, 5, clues)
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