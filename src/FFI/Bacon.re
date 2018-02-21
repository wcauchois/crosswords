type domElement = Dom.domElement;

type observable('a);

[@bs.val] [@bs.module "baconjs"]
external fromEvent : (domElement, string) => observable('a) = "";

[@bs.val] [@bs.module "baconjs"] external once : 'a => observable('a) = "";

[@bs.val] [@bs.module "baconjs"] external never : unit => observable('a) = "";

[@bs.val] [@bs.module "baconjs"]
external fromCallback : (('a => unit) => unit) => observable('a) = "";

[@bs.val] [@bs.module "baconjs"]
external repeat_ : (int => observable('a)) => observable('a) = "repeat";

/* Wrapper is necessary due to @bs.unwrap not working in callbacks.
   https://github.com/BuckleScript/bucklescript/issues/2292 */
let repeat: (int => option(observable('a))) => observable('a) =
  callback =>
    repeat_(i =>
      switch (callback(i)) {
      | Some(result) => result
      | None => [%bs.raw {| undefined |}]
      }
    );

module Observable = {
  [@bs.send] external onValue : (observable('a), 'a => unit) => unit = "";
  [@bs.send] external map : (observable('a), 'a => 'b) => observable('b) = "";
  [@bs.send]
  external flatMap : (observable('a), 'a => observable('b)) => observable('b) =
    "";
  [@bs.send]
  external scan : (observable('a), 'b, ('b, 'a) => 'b) => observable('b) = "";
  let flatMapOption =
      (obs: observable('a), fn: 'a => option('b))
      : observable('b) =>
    flatMap(obs, item =>
      switch (fn(item)) {
      | Some(ret) => once(ret)
      | None => never()
      }
    );
};

/* This could be implemented with Bacon.fromEvent, i.e.:
        Bacon.fromEvent(Dom.getByTagName("body")[0], "keydown")
   But this on the other hand also prevents the default action of the event.
   */
let capturingKeyboardObservable: unit => observable(Dom.KeyboardEvent.t) =
  () => {
    let currentCallback: ref(option(Dom.KeyboardEvent.t => unit)) = ref(None);
    Dom.Window.addEventListener("keydown", event => {
      Dom.KeyboardEvent.preventDefault(event);
      switch currentCallback^ {
      | Some(callback) => callback(event)
      | None => ()
      };
    });
    repeat(_i =>
      Some(fromCallback(callback => currentCallback := Some(callback)))
    );
  };