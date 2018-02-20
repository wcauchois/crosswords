// Generated by BUCKLESCRIPT VERSION 2.2.0, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Baconjs = require("baconjs");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Dom$Crosswords = require("./FFI/Dom.bs.js");
var Util$Crosswords = require("./Util.bs.js");
var Bacon$Crosswords = require("./FFI/Bacon.bs.js");
var Board$Crosswords = require("./Board.bs.js");

var canvas = Curry._1(Dom$Crosswords.getById, "c");

var context = canvas.getContext("2d");

var b = Board$Crosswords.setState(5, 5, /* Blocked */1, Board$Crosswords.setState(0, 0, /* Blocked */1, Board$Crosswords.empty(10, 10)));

var board = Board$Crosswords.setModifier(2, 2, /* PrimaryHighlighted */0, List.fold_left((function (b, param) {
            return Board$Crosswords.setState(1 + param[0] | 0, 2, /* Full */[param[1]], b);
          }), b, List.mapi((function (i, c) {
                return /* tuple */[
                        i,
                        c
                      ];
              }), Util$Crosswords.explodeString("hello"))));

Board$Crosswords.draw(board, context);

var obs = Baconjs.fromEvent(Caml_array.caml_array_get(Curry._1(Dom$Crosswords.getByTagName, "body"), 0), "keydown");

var keyObs = Bacon$Crosswords.Observable[/* flatMapOption */0](obs, (function ($$event) {
        var match = $$event.key;
        switch (match) {
          case "ArrowDown" : 
              return /* Some */[/* Down */3];
          case "ArrowLeft" : 
              return /* Some */[/* Left */0];
          case "ArrowRight" : 
              return /* Some */[/* Right */1];
          case "ArrowUp" : 
              return /* Some */[/* Up */2];
          default:
            return /* None */0;
        }
      }));

var selObs = keyObs.scan(/* tuple */[
      0,
      0
    ], (function (param, key) {
        var ySel = param[1];
        var xSel = param[0];
        var match;
        switch (key) {
          case 0 : 
              match = /* tuple */[
                xSel - 1 | 0,
                ySel + 0 | 0
              ];
              break;
          case 1 : 
              match = /* tuple */[
                xSel + 1 | 0,
                ySel + 0 | 0
              ];
              break;
          case 2 : 
              match = /* tuple */[
                xSel + 0 | 0,
                ySel - 1 | 0
              ];
              break;
          case 3 : 
              match = /* tuple */[
                xSel + 0 | 0,
                ySel + 1 | 0
              ];
              break;
          
        }
        return /* tuple */[
                match[0],
                match[1]
              ];
      }));

selObs.onValue((function (param) {
        console.log(/* int array */[
              param[0],
              param[1]
            ]);
        return /* () */0;
      }));

var Ctx = 0;

var Observable = 0;

var KeyboardEvent = 0;

var currentSel = /* tuple */[
  0,
  0
];

exports.canvas = canvas;
exports.context = context;
exports.Ctx = Ctx;
exports.board = board;
exports.Observable = Observable;
exports.KeyboardEvent = KeyboardEvent;
exports.obs = obs;
exports.keyObs = keyObs;
exports.currentSel = currentSel;
exports.selObs = selObs;
/* canvas Not a pure module */
