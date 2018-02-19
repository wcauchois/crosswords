type domElement = Dom.domElement;

type observable('a);

type event('a);

[@bs.val] [@bs.module "baconjs"]
external fromEvent : (domElement, string) => observable(unit) = "";

module Observable = {
  [@bs.send]
  external onValue : (observable('a), event('a) => unit) => unit = "";
};