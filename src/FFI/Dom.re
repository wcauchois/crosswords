type domElement;

let getById: string => domElement = [%bs.raw
  {|
    function(arg) {
      return document.getElementById(arg);
    }
  |}
];
