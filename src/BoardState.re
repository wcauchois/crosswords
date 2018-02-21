type orientation =
  | Horizontal
  | Vertical;

type t = {
  cursor: (int, int),
  orientation
};

let flipOrientation: orientation => orientation =
  o =>
    switch o {
    | Horizontal => Vertical
    | Vertical => Horizontal
    };

let empty: Board.t => t =
  b => {
    let firstFreeCell: (int, int) =
      List.find(
        ((x, y)) =>
          switch (Board.getState(x, y, b)) {
          | Board.EmptyCell => true
          | _ => false
          },
        Board.list_of_coords(b)
      );
    {cursor: firstFreeCell, orientation: Horizontal};
  };

let direction_of_orientation = (o: orientation) =>
  switch o {
  | Horizontal => (1, 0)
  | Vertical => (0, 1)
  };

let applyModifiers: (Board.t, t) => Board.t =
  (b, s) => {
    let (cursorX, cursorY) = s.cursor;
    let (dirX, dirY) = direction_of_orientation(s.orientation);
    let packedMods =
      List.map(
        (modScalar, b: Board.t) => {
          let positionsToFill: list((int, int)) =
            Util.unfold(
              i => {
                let (x, y) = (
                  dirX * modScalar * i + cursorX,
                  dirY * modScalar * i + cursorY
                );
                if (Board.isValidCoord(x, y, b)) {
                  switch (Board.get(x, y, b)) {
                  | (Board.Blocked, _) => None
                  | _ => Some(((x, y), i + 1))
                  };
                } else {
                  None;
                };
              },
              1
            );
          List.fold_left(
            (b, (x, y)) =>
              Board.setModifier(x, y, Board.SecondaryHighlighted, b),
            b,
            positionsToFill
          );
        },
        [(-1), 1]
      );
    let (negMod, posMod) =
      switch packedMods {
      | [negMod, posMod] => (negMod, posMod)
      | _ => raise(Failure("Shouldn't happen"))
      };
    Board.setModifier(cursorX, cursorY, Board.PrimaryHighlighted, b)
    |> negMod
    |> posMod;
  };