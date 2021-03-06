// Generated by BUCKLESCRIPT VERSION 2.2.0, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");
var Dom$Crosswords = require("./FFI/Dom.bs.js");
var Util$Crosswords = require("./Util.bs.js");
var Bacon$Crosswords = require("./FFI/Bacon.bs.js");
var Board$Crosswords = require("./Board.bs.js");
var BoardState$Crosswords = require("./BoardState.bs.js");

var canvas = Curry._1(Dom$Crosswords.getById, "c");

var context = canvas.getContext("2d");

var clues_000 = /* record */[
  /* x */1,
  /* y */0,
  /* t */"What everyone brings to a potluck dinner",
  /* o : Horizontal */0,
  /* a */"DISH"
];

var clues_001 = /* :: */[
  /* record */[
    /* x */1,
    /* y */1,
    /* t */"Swimming lane separator",
    /* o : Horizontal */0,
    /* a */"ROPE"
  ],
  /* :: */[
    /* record */[
      /* x */0,
      /* y */2,
      /* t */"Two-faced Roman god",
      /* o : Horizontal */0,
      /* a */"JANUS"
    ],
    /* :: */[
      /* record */[
        /* x */0,
        /* y */3,
        /* t */"Similar (to)",
        /* o : Horizontal */0,
        /* a */"AKIN"
      ],
      /* :: */[
        /* record */[
          /* x */0,
          /* y */4,
          /* t */"___ Bennett, \"S.N.L.\" cast member who impersonates Putin",
          /* o : Horizontal */0,
          /* a */"BECK"
        ],
        /* :: */[
          /* record */[
            /* x */0,
            /* y */2,
            /* t */"Quick punch",
            /* o : Vertical */1,
            /* a */"JAB"
          ],
          /* :: */[
            /* record */[
              /* x */1,
              /* y */0,
              /* t */"\"Started From the Bottom\" rapper",
              /* o : Vertical */1,
              /* a */"DRAKE"
            ],
            /* :: */[
              /* record */[
                /* x */2,
                /* y */0,
                /* t */"Kind of bond in chemistry",
                /* o : Vertical */1,
                /* a */"IONIC"
              ],
              /* :: */[
                /* record */[
                  /* x */3,
                  /* y */0,
                  /* t */"Courageous spirit",
                  /* o : Vertical */1,
                  /* a */"SPUNK"
                ],
                /* :: */[
                  /* record */[
                    /* x */4,
                    /* y */0,
                    /* t */"___ Just Not That Into You",
                    /* o : Vertical */1,
                    /* a */"HES"
                  ],
                  /* [] */0
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
];

var clues = /* :: */[
  clues_000,
  clues_001
];

var blocks = /* :: */[
  /* tuple */[
    0,
    0
  ],
  /* :: */[
    /* tuple */[
      0,
      1
    ],
    /* :: */[
      /* tuple */[
        4,
        3
      ],
      /* :: */[
        /* tuple */[
          4,
          4
        ],
        /* [] */0
      ]
    ]
  ]
];

var b = Board$Crosswords.empty(5, 5, clues);

var initBoard = List.fold_left((function (b, param) {
        return Board$Crosswords.setState(param[0], param[1], /* Blocked */1, b);
      }), b, blocks);

Board$Crosswords.draw(initBoard, context);

var keyObs = Bacon$Crosswords.capturingKeyboardObservable((function ($$event) {
        var s = $$event.key;
        switch (s) {
          case " " : 
              return /* Some */[/* SpaceBar */4];
          case "ArrowDown" : 
              return /* Some */[/* Down */3];
          case "ArrowLeft" : 
              return /* Some */[/* Left */0];
          case "ArrowRight" : 
              return /* Some */[/* Right */1];
          case "ArrowUp" : 
              return /* Some */[/* Up */2];
          default:
            if (s.length === 1 && Util$Crosswords.isAlpha(s)) {
              return /* Some */[/* Alpha */[$$String.uppercase(s)]];
            } else {
              return /* None */0;
            }
        }
      }));

var stateObs = keyObs.scan(BoardState$Crosswords.empty(initBoard), (function (s, key) {
        var moveDirectionOpt;
        if (typeof key === "number") {
          switch (key) {
            case 0 : 
                moveDirectionOpt = /* Some */[/* tuple */[
                    -1,
                    0
                  ]];
                break;
            case 1 : 
                moveDirectionOpt = /* Some */[/* tuple */[
                    1,
                    0
                  ]];
                break;
            case 2 : 
                moveDirectionOpt = /* Some */[/* tuple */[
                    0,
                    -1
                  ]];
                break;
            case 3 : 
                moveDirectionOpt = /* Some */[/* tuple */[
                    0,
                    1
                  ]];
                break;
            case 4 : 
                moveDirectionOpt = /* None */0;
                break;
            
          }
        } else {
          moveDirectionOpt = /* None */0;
        }
        var stateWithCursor;
        if (moveDirectionOpt) {
          var match = moveDirectionOpt[0];
          stateWithCursor = BoardState$Crosswords.moveCursor(match[0], match[1], initBoard, s);
        } else {
          stateWithCursor = s;
        }
        var newOrientation = key === /* SpaceBar */4 ? BoardState$Crosswords.flipOrientation(s[/* orientation */1]) : s[/* orientation */1];
        return /* record */[
                /* cursor */stateWithCursor[/* cursor */0],
                /* orientation */newOrientation
              ];
      }));

var boardObs = keyObs.combine(stateObs, (function (key, state) {
          return /* tuple */[
                  key,
                  state
                ];
        })).scan(initBoard, (function (b, param) {
        var key = param[0];
        var match = param[1][/* cursor */0];
        if (typeof key === "number") {
          return b;
        } else {
          return Board$Crosswords.setState(match[0], match[1], /* Full */[Caml_string.get(key[0], 0)], b);
        }
      }));

var boardAndStateObs = boardObs.combine(stateObs, (function (b, s) {
          return /* tuple */[
                  b,
                  s
                ];
        })).map((function (param) {
        var s = param[1];
        return /* tuple */[
                BoardState$Crosswords.applyModifiers(param[0], s),
                s
              ];
      }));

boardAndStateObs.onValue((function (param) {
        var clue = Util$Crosswords.getOrThrowDefault(BoardState$Crosswords.currentClue(param[0], param[1]));
        var clueElem = Curry._1(Dom$Crosswords.getById, "clue");
        clueElem.textContent = clue[/* t */2];
        return /* () */0;
      }));

boardAndStateObs.onValue((function (param) {
        context.clearRect(0.0, 0.0, 480.0, 480.0);
        return Board$Crosswords.draw(param[0], context);
      }));

var Ctx = 0;

var across = /* Horizontal */0;

var down = /* Vertical */1;

var Observable = 0;

var KeyboardEvent = 0;

exports.canvas = canvas;
exports.context = context;
exports.Ctx = Ctx;
exports.across = across;
exports.down = down;
exports.clues = clues;
exports.blocks = blocks;
exports.initBoard = initBoard;
exports.Observable = Observable;
exports.KeyboardEvent = KeyboardEvent;
exports.keyObs = keyObs;
exports.stateObs = stateObs;
exports.boardObs = boardObs;
exports.boardAndStateObs = boardAndStateObs;
/* canvas Not a pure module */
