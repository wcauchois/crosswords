let canvas = Canvas.getById("c");

let context = Canvas.getContext(canvas);

module Ctx = Canvas.Ctx;

Ctx.setFillStyle(context, "#f00");

Ctx.fillRect(context, 0.0, 0.0, 100.0, 100.0);