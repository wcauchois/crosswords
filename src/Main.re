let canvas = Dom.getById("c");

let context = Canvas.getContext(canvas);

module Ctx = Canvas.Ctx;

let board =
  Board.empty(10, 10)
  |> Board.setState(0, 0, Board.Blocked)
  |> Board.setState(5, 5, Board.Blocked)
  |> (
    (b: Board.t) =>
      List.fold_left(
        (b: Board.t, (i, c): (int, char)) =>
          Board.setState(1 + i, 2, Board.Full(c), b),
        b,
        List.mapi((i, c) => (i, c), Util.explodeString("hello"))
      )
  )
  |> Board.setModifier(2, 2, Board.PrimaryHighlighted);

Board.draw(board, context);

module Observable = Bacon.Observable;

module KeyboardEvent = Dom.KeyboardEvent;

type keyboardInput =
  | Left
  | Right
  | Up
  | Down;

let keyObs: Bacon.observable(keyboardInput) =
  Bacon.capturingKeyboardObservable(event =>
    switch (KeyboardEvent.key(event)) {
    | "ArrowUp" => Some(Up)
    | "ArrowDown" => Some(Down)
    | "ArrowLeft" => Some(Left)
    | "ArrowRight" => Some(Right)
    | _ => None
    }
  );

let currentSel = (0, 0);

let selObs =
  Observable.scan(
    keyObs,
    (0, 0),
    ((xSel, ySel), key) => {
      let (unclampedX, unclampedY) =
        switch key {
        | Up => (xSel + 0, ySel - 1)
        | Down => (xSel + 0, ySel + 1)
        | Left => (xSel - 1, ySel + 0)
        | Right => (xSel + 1, ySel + 0)
        };
      (unclampedX, unclampedY);
    }
  );

Observable.onValue(selObs, ((xSel, ySel)) => Js.log([|xSel, ySel|]));

let boardObs =
  Observable.map(selObs, ((xSel, ySel)) =>
    Board.setModifier(xSel, ySel, Board.SecondaryHighlighted, board)
  );

Observable.onValue(
  boardObs,
  b => {
    Canvas.Ctx.clearRect(context, 0.0, 0.0, 480.0, 480.0);
    Board.draw(b, context);
  }
);
/*Observable.onValue(keyObs, k => {
    Js.log(k);
  });*/
/*Bacon.Observable.onValue(obs, (x) => {
    Js.log(x);
    Bacon.never();
    ();
  });
  */
/*Ctx.setFillStyle(context, "#f00");

  Ctx.setFont(context, "48px sans-serif");

  Ctx.fillText(context, "hello", 0.0, 48.0);*/