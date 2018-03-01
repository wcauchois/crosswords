open Jest;

open Expect;

open Util;

describe("isAlpha", () =>
  testAll(
    "all",
    [
      ("a", true),
      ("D", true),
      ("-", false),
      (".", false),
      ("", false),
      (" ", false)
    ],
    ((s, b)) =>
    expect(isAlpha(s)) |> toEqual(b)
  )
);