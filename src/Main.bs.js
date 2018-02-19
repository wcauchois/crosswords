// Generated by BUCKLESCRIPT VERSION 2.2.0, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Baconjs = require("baconjs");
var Caml_string = require("bs-platform/lib/js/caml_string.js");
var Dom$Crosswords = require("./FFI/Dom.bs.js");
var Board$Crosswords = require("./Board.bs.js");

var canvas = Curry._1(Dom$Crosswords.getById, "c");

var context = canvas.getContext("2d");

function explodeString(s) {
  var s$1 = s;
  var _idx = s.length - 1 | 0;
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var idx = _idx;
    if (idx === -1) {
      return acc;
    } else {
      var c = Caml_string.get(s$1, idx);
      _acc = /* :: */[
        c,
        acc
      ];
      _idx = idx - 1 | 0;
      continue ;
      
    }
  };
}

var b = Board$Crosswords.setState(5, 5, /* Blocked */1, Board$Crosswords.setState(0, 0, /* Blocked */1, Board$Crosswords.empty(10, 10)));

var board = Board$Crosswords.setModifier(2, 2, /* PrimaryHighlighted */0, List.fold_left((function (b, param) {
            return Board$Crosswords.setState(1 + param[0] | 0, 2, /* Full */[param[1]], b);
          }), b, List.mapi((function (i, c) {
                return /* tuple */[
                        i,
                        c
                      ];
              }), explodeString("hello"))));

Board$Crosswords.draw(board, context);

var obs = Baconjs.fromEvent(canvas, "click");

obs.onValue((function () {
        console.log("got click event");
        return /* () */0;
      }));

var Ctx = 0;

exports.canvas = canvas;
exports.context = context;
exports.Ctx = Ctx;
exports.explodeString = explodeString;
exports.board = board;
exports.obs = obs;
/* canvas Not a pure module */
