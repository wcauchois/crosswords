let canvas = Canvas.getById("c");

let context = Canvas.getContext(canvas);

module Ctx = Canvas.Ctx;

let explodeString = (s: string) : list(char) => {
  let rec explodeHelper = (s: string, idx: int, acc: list(char)) =>
    if (idx == (-1)) {
      acc;
    } else {
      let c = s.[idx];
      explodeHelper(s, idx - 1, [c, ...acc]);
    };
  explodeHelper(s, String.length(s) - 1, []);
};

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
        List.mapi((i, c) => (i, c), explodeString("hello"))
      )
  )
  |> Board.setModifier(2, 2, Board.PrimaryHighlighted);

Board.draw(board, context);
/*Ctx.setFillStyle(context, "#f00");

  Ctx.setFont(context, "48px sans-serif");

  Ctx.fillText(context, "hello", 0.0, 48.0);*/