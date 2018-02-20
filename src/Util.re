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