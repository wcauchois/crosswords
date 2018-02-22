// Generated by BUCKLESCRIPT VERSION 2.2.0, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");

var getById = (
    function(arg) {
      return document.getElementById(arg);
    }
  );

var getByTagName = (
    function(arg) {
      return document.getElementsByTagName(arg);
    }
  );

function addEventListener(eventName, callback) {
  var jsFunc = (
          function(eventName, callback) {
            window.addEventListener(eventName, callback);
          }
        );
  return Curry._2(jsFunc, eventName, callback);
}

var Window = /* module */[/* addEventListener */addEventListener];

var Element = /* module */[];

var KeyboardEvent = /* module */[];

exports.getById = getById;
exports.getByTagName = getByTagName;
exports.Window = Window;
exports.Element = Element;
exports.KeyboardEvent = KeyboardEvent;
/* getById Not a pure module */
