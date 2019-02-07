// Generated by BUCKLESCRIPT VERSION 4.0.18, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var List = require("bs-platform/lib/js/list.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");

var text = Fs.readFileSync("test.txt", "utf8");

var re = new RegExp("(^.+$)", "gm");

function foo(param) {
  var match = re.exec(text);
  if (match !== null) {
    var match$1 = Caml_array.caml_array_get(match, 0);
    if (match$1 == null) {
      return /* [] */0;
    } else {
      return /* :: */[
              match$1,
              foo(/* () */0)
            ];
    }
  } else {
    return /* [] */0;
  }
}

var lines = foo(/* () */0);

var ints = List.map(Caml_format.caml_int_of_string, lines);

var sum = List.fold_left((function (a, b) {
        return a + b | 0;
      }), 0, ints);

console.log(sum);

exports.text = text;
exports.re = re;
exports.foo = foo;
exports.lines = lines;
exports.ints = ints;
exports.sum = sum;
/* text Not a pure module */
