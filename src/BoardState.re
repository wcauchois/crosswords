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

let filledCoords: (Board.t, t) => list((int, int)) =
  (b, s) => {
    let (cursorX, cursorY) = s.cursor;
    let (dirX, dirY) = direction_of_orientation(s.orientation);
    let nonCursorPositions =
      List.flatten(
        List.map(
          modScalar =>
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
            ),
          [(-1), 1]
        )
      );
    [s.cursor, ...nonCursorPositions];
  };

module PairsMap = Board.PairsMap;

let currentClue: (Board.t, t) => option(string) =
  (b, s) => {
    let clueCoordOpt =
      try (
        Some(List.find(coord => PairsMap.mem(coord, b.clues), filledCoords(b, s)))
      ) {
      | Not_found => None
      };
    Js.Option.map([@bs] clueCoord => PairsMap.find(clueCoord, b.clues), clueCoordOpt)
  };

let applyModifiers: (Board.t, t) => Board.t =
  (b, s) => {
    let (cursorX, cursorY) = s.cursor;
    Board.setModifier(cursorX, cursorY, Board.PrimaryHighlighted, b)
    |> (
      (b: Board.t) =>
        List.fold_left(
          (b, (x, y)) =>
            Board.setModifier(x, y, Board.SecondaryHighlighted, b),
          b,
          List.filter(c => c != s.cursor, filledCoords(b, s))
        )
    );
  };