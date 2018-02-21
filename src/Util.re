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

/* Dual of fold. While fold reduces a list to a summary value,
   unfold builds a list from a seed value.
   See https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-List.html#v:unfoldr */
let unfold: ('b => option(('a, 'b)), 'b) => list('a) =
  (f, seed) => {
    let rec unfoldRec = (current: 'b, acc: list('a)) =>
      switch (f(current)) {
      | Some((value, newCurrent)) => unfoldRec(newCurrent, [value, ...acc])
      | None => List.rev(acc)
      };
    unfoldRec(seed, []);
  };