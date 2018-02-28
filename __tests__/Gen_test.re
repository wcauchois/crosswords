open Jest;

open Expect;

open Gen;

let exampleList = [1, 2, 3];

describe("ofList", () => {
  let gen = ofList(exampleList);
  testAll(
    "generator", List.append(List.map(i => Some(i), exampleList), [None]), iOpt =>
    expect(gen()) |> toEqual(iOpt)
  );
});

describe("collect", () => {
  let gen = ofList(exampleList);
  test("roundTrip", () =>
    expect(collect(gen)) |> toEqual(exampleList)
  );
});

describe("range", () => {
  test("simple", () =>
    expect(collect(range(0, 3))) |> toEqual([0, 1, 2])
  );
  test("step", () =>
    expect(collect(rangeStep(0, 5, 2))) |> toEqual([0, 2, 4])
  );
  test("negativeStep", () =>
    expect(collect(rangeStep(3, 0, -1))) |> toEqual([3, 2, 1])
  );
});

describe("cartesian", () => {
  test("simple", () =>
    expect(collect(cartesian(ofList([1, 2]), ofList([3, 4]))))
    |> toEqual([(1, 3), (1, 4), (2, 3), (2, 4)])
  );
  test("emptyFirstList", () =>
    expect(collect(cartesian(ofList([1, 2]), () => None))) |> toEqual([])
  );
  test("emptySecondList", () =>
    expect(collect(cartesian(() => None, ofList([1, 2])))) |> toEqual([])
  );
});

describe("find", () => {
  test("withResult", () =>
    expect(find(x => x == 2, ofList([1, 2, 3]))) |> toEqual(Some(2))
  );
  test("noResult", () =>
    expect(find(x => x == 10, ofList([1, 2, 3]))) |> toEqual(None)
  );
});

describe("map", () =>
  test("simple", () =>
    expect(ofList([1, 2, 3]) |> map(x => x * 2) |> collect)
    |> toEqual([2, 4, 6])
  )
);