type domElement = Dom.domElement;

type observable('a);

type keyboardEvent;

[@bs.val] [@bs.module "baconjs"]
external fromEvent : (domElement, string) => observable('a) = "";

[@bs.val] [@bs.module "baconjs"]
external once : 'a => observable('a) = "";

[@bs.val] [@bs.module "baconjs"]
external never : unit => observable('a) = "";

module KeyboardEvent {
  [@bs.get] external key : keyboardEvent => string = "";
  [@bs.get] external keyCode : keyboardEvent => int = "";
};

module Observable = {
  [@bs.send]
  external onValue : (observable('a), 'a => unit) => unit = "";

  [@bs.send]
  external map : (observable('a), 'a => 'b) => observable('b) = "";

  [@bs.send]
  external flatMap : (observable('a), 'a => observable('b)) => observable('b) = "";

  [@bs.send]
  external scan : (observable('a), 'b, ('b, 'a) => 'b) => observable('b) = "";
};