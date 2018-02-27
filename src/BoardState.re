type orientation = Util.orientation;

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

/* Precondition: only one of dirX or dirY is nonzero. */
let moveCursor: (int, int, Board.t, t) => t =
  (dirX, dirY, b, s) => {
    s
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

let filledCoords: (Board.t, t) => list((int, int)) =
  (b, s) => {
    let (cursorX, cursorY) = s.cursor;
    let (dirX, dirY) = Util.direction_of_orientation(s.orientation);
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

let clueForOrientation =
    (p: Board.cluePair, o: orientation)
    : option(Board.clue) =>
  switch o {
  | Horizontal => p.horizontal
  | Vertical => p.vertical
  };

let currentClue: (Board.t, t) => option(Board.clue) =
  (b, s) => {
    let clueCoordOpt =
      try (
        Some(
          List.find(
            coord =>
              switch (PairsMap.find(coord, b.clues)) {
              | pair =>
                Js.Option.isSome(clueForOrientation(pair, s.orientation))
              | exception Not_found => false
              },
            filledCoords(b, s)
          )
        )
      ) {
      | Not_found => None
      };
    Util.flattenOption(
      Js.Option.map(
        [@bs]
        (
          clueCoord =>
            clueForOrientation(
              PairsMap.find(clueCoord, b.clues),
              s.orientation
            )
        ),
        clueCoordOpt
      )
    );
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