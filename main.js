////////////////////////////////////////////////////////////////////////////////
// Helpers

// [min, max)
function randInt(min, max) {
  return Math.floor(Math.random() * (max - min)) + min;
}

function randFloat(min, max) {
  return Math.random() * (max - min) + min;
}

////////////////////////////////////////////////////////////////////////////////
// Globals

const bubbleCount = 100;

const baseSpeed = 20;
const speedVariation = 15;

const baseRadius = 50;
const radiusVariation = 15;

let canvas, ctx, points;
let previousTimestamp, delta;

////////////////////////////////////////////////////////////////////////////////
// Physics

function setPoints() {
  points = [];
  for (let i = 0; i < bubbleCount; i++) {
    const radius = randFloat(
      baseRadius - radiusVariation,
      baseRadius + radiusVariation
    );
    points.push({
      "x": randInt(radius, window.innerWidth - radius),
      "y": randInt(radius, window.innerHeight - radius),
      "angle": randFloat(0, 2 * Math.PI),
      "speed": randFloat(baseSpeed - speedVariation, baseSpeed + speedVariation),
      "radius": radius,
    });
  }
}

function update(delta) {
  for (let point of points) {
    point.x += Math.cos(point.angle) * point.speed * delta;
    point.y += Math.sin(point.angle) * point.speed * delta;

    if (point.x > window.innerWidth - point.radius) {
      point.x = window.innerWidth - point.radius;
      point.angle = Math.PI - point.angle;
    }

    if (point.x < point.radius) {
      point.x = point.radius;
      point.angle = Math.PI - point.angle;
    }

    if (point.y > window.innerHeight - point.radius) {
      point.y = window.innerHeight - point.radius;
      point.angle = -point.angle;
    }

    if (point.y < point.radius) {
      point.y = point.radius;
      point.angle = -point.angle;
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
// Rendering

function resetCanvas() {
  canvas.width = window.innerWidth;
  canvas.height = window.innerHeight;
  ctx.lineWidth = 1;
  ctx.strokeStyle = "rgba(0, 0, 0, 0.1)";
}

function render() {
  ctx.clearRect(0, 0, window.innerWidth, window.innerHeight);
  for (let point of points) {
    ctx.beginPath();
    ctx.arc(point.x, point.y, point.radius, 0, 2 * Math.PI, false);
    ctx.stroke();
  }
}

////////////////////////////////////////////////////////////////////////////////
// Main functions

function step(timestamp) {
  const delta = previousTimestamp ? (timestamp - previousTimestamp) / 1000 : 0;
  previousTimestamp = timestamp;

  update(delta);

  render();

  if (window.matchMedia("(prefers-reduced-motion: no-preference)").matches) {
    window.requestAnimationFrame(step);
  }
}

window.addEventListener("load", (_) => {
  // // Light/dark mode

  // const mode = document.querySelector("#mode");
  // const modeInput = mode.querySelector("input");

  // if (previousTheme === "dark") {
  //   modeInput.checked = true;
  // }

  // window.setTimeout(() => {
  //   mode.classList.add("ready");
  // }, 50);

  // modeInput.addEventListener("change", (_) => {
  //   if (modeInput.checked) {
  //     document.documentElement.classList.add("dark");
  //     document.documentElement.classList.remove("light");
  //     localStorage.setItem("theme", "dark");
  //   } else {
  //     document.documentElement.classList.add("light");
  //     document.documentElement.classList.remove("dark");
  //     localStorage.setItem("theme", "light");
  //   }
  // });

  // Bubbles

  canvas = document.getElementById("background-canvas");
  ctx = canvas.getContext("2d");

  resetCanvas();
  setPoints();

  window.addEventListener("resize", (_) => {
    resetCanvas();
  });

  window.requestAnimationFrame(step);
});

// Immediately load previous light/dark theme

// const previousTheme = localStorage.getItem("theme");
// if (previousTheme === "dark") {
//   document.documentElement.classList.add("dark");
// } else if (previousTheme === "light") {
//   document.documentElement.classList.add("light");
// }
