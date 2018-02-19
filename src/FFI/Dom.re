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