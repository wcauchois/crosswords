type domElement;

let getById: string => domElement = [%bs.raw
  {|
    function(arg) {
      return document.getElementById(arg);
    }
  |}
];

let getByTagName: string => array(domElement) = [%bs.raw
  {|
    function(arg) {
      return document.getElementsByTagName(arg);
    }
  |}
];

module Window = {
  let addEventListener: (string, 'a => unit) => unit =
    (eventName, callback) => {
      let jsFunc: (string, 'a => unit) => unit = [%bs.raw
        {|
          function(eventName, callback) {
            window.addEventListener(eventName, callback);
          }
        |}
      ];
      jsFunc(eventName, callback);
    };
};

module Element {
  [@bs.set] external setTextContent : (domElement, string) => unit = "textContent";
};

module KeyboardEvent = {
  type t;
  [@bs.send] external preventDefault : t => unit = "";
  [@bs.get] external key : t => string = "";
  [@bs.get] external keyCode : t => int = "";
};