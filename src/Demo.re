let score: int = 10;

let add = (x: int, y: int) : int => x + y;

add(1, 2);

type myVariant =
  | Yes
  | No(int)
  | Complex(string, int);

let variantValue: myVariant = Yes;

let getVariantValue = (x: myVariant) : int =>
  switch x {
  | Yes => 0
  | No(i) => i
  | Complex(s, i) => int_of_string(s) + i
  };

Js.log("Hello, BuckleScript and Reason!");

let x: array(int) = Array.map((x: int) : int => x + 1, [|1, 2, 3|]);

print_endline("another thing: ");
Js.log(x);
