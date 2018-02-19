module IntPairs = {
  type t = (int, int);
  let compare = ((x0, y0): t, (x1, y1): t) =>
    switch (Pervasives.compare(x0, x1)) {
    | 0 => Pervasives.compare(y0, y1)
    | c => c
    };
};

module PairsMap = Map.Make(IntPairs);

let boardScale: float = 32.0;

let gutterSize: float = 1.0;

let (fontSizeMod, fontOffsX, fontOffsY) = (1.0, 8.0, (-4.0));

let fontFace: string = "monospace";

let primaryFillColor = "#ffeb3b";

let secondaryFillColor = "#2196f3";

type cellState =
  | EmptyCell
  | Full(char)
  | Blocked;

type cellModifier =
  | PrimaryHighlighted
  | SecondaryHighligted
  | Unmodified;

type cell = (cellState, cellModifier);

type t = {
  width: int,
  height: int,
  cells: PairsMap.t(cell)
};

let empty = (width: int, height: int) : t => {
  width,
  height,
  cells: PairsMap.empty
};

let get = (x: int, y: int, b: t) : cell =>
  try (PairsMap.find((x, y), b.cells)) {
  | Not_found => (EmptyCell, Unmodified)
  };

let makeModFn = (modFn: (cell, 'a) => cell) : ((int, int, 'a, t) => t) =>
  (x: int, y: int, value: 'a, b: t) => {
    ...b,
    cells:
      switch (get(x, y, b)) {
      | currentCell => PairsMap.add((x, y), modFn(currentCell, value), b.cells)
      }
  };

let setState = makeModFn(((_, prevModifier), value) => (value, prevModifier));

let setModifier = makeModFn(((prevState, _), value) => (prevState, value));

module Ctx = Canvas.Ctx;

let draw = (b: t, context: Canvas.ctx) : unit => {
  let fontSize = boardScale *. fontSizeMod;
  let roundedFontSize = int_of_float(floor(fontSize));
  Ctx.setFont(context, {j|$(roundedFontSize)px $(fontFace)|j});
  Ctx.setStrokeWidth(context, 1.0);
  let drawHorizRulerAt = (drawY: float) =>
    Ctx.line(
      context,
      (0.0, drawY),
      (float_of_int(b.width) *. (boardScale +. gutterSize), drawY)
    );
  let drawVertRulerAt = (drawX: float) =>
    Ctx.line(
      context,
      (drawX, 0.0),
      (drawX, float_of_int(b.height) *. (boardScale +. gutterSize))
    );
  List.iter(f => f(0.0), [drawHorizRulerAt, drawVertRulerAt]);
  for (y in 0 to b.height) {
    for (x in 0 to b.width) {
      let xFloat = float_of_int(x);
      let yFloat = float_of_int(y);
      let drawX = xFloat *. boardScale +. (xFloat -. 1.0) *. gutterSize;
      let drawY = yFloat *. boardScale +. (yFloat -. 1.0) *. gutterSize;
      drawHorizRulerAt(drawY);
      drawVertRulerAt(drawX);
      let (state, modifier) = get(x, y, b);
      let maybeFillColor =
        switch modifier {
        | Unmodified => None
        | PrimaryHighlighted => Some(primaryFillColor)
        | SecondaryHighligted => Some(secondaryFillColor)
        };
      switch maybeFillColor {
      | None => ()
      | Some(fillColor) =>
        Ctx.setFillStyle(context, fillColor);
        Ctx.fillRect(context, drawX, drawY, boardScale, boardScale);
      };
      switch state {
      | EmptyCell => ()
      | Full(letter) =>
        Ctx.setFillStyle(context, "#000");
        Ctx.fillText(
          context,
          String.make(1, letter),
          drawX +. fontOffsX,
          drawY +. 32.0 +. fontOffsY
        );
      | Blocked =>
        Ctx.setFillStyle(context, "#000");
        Ctx.fillRect(context, drawX, drawY, boardScale, boardScale);
      };
    };
  };
};
/*let setCell = (b: board, x: int, y: int, newValue: cell) : board => {

  };
  */