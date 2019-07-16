(function() {

"use strict";

var canvas, ctx;

function init(width, height) {
  canvas = document.getElementById("game");
  canvas.width = width;
  canvas.height = height;

  ctx = canvas.getContext("2d");
}

function drawRectangle(x, y, width, height, color) {
  ctx.fillStyle = color;
  ctx.fillRect(
    Math.round(x), Math.round(y), Math.round(width), Math.round(height)
  );
}

function draw(renderables) {
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  for (let i = renderables.length - 1; i >= 0; i--) {
    let r = renderables[i];
    switch (r.kind) {
      case "rectangle":
        drawRectangle(r.x, r.y, r.width, r.height, r.color);
        break;
      default:
        console.warn("draw(): unknown renderable");
        break;
    }
  }
}

app.ports.canvas.subscribe(function(data) {
  let name = data.name;
  let args = data.args;

  switch (name) {
    case "init":
      init(args.width, args.height);
      break;
    case "draw":
      draw(args.renderables);
      break;
    default:
      console.warn("canvas subscription: unknown command");
      break;
  }
});

})();
