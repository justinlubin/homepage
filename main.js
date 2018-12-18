/*
 * This program uses the following MIT-licensed library for computing the
 * Voronoi diagram:
 *   https://github.com/gorhill/Javascript-Voronoi
 */

(function() {
  "use strict";

  // [min, max)
  function randInt(min, max) {
    return Math.floor(Math.random() * (max - min)) + min;
  }

  function point(x, y, angle) {
    return {
      "x": x,
      "y": y,
      "angle": angle
    };
  }

  function color(strokeStyle) {
    return {
      "strokeStyle": strokeStyle
    };
  }

  var mainCanvas, ctx, bbox, points, colors;
  var previousTimestamp, delta;

  var playPauseButton;
  var isPlaying = true;

  function render() {
    ctx.clearRect(0, 0, bbox.xr, bbox.yb);

    var voronoi = new Voronoi();
    var diagram = voronoi.compute(points, bbox);
    var cells = diagram.cells;

    for (var c = 0; c < cells.length; c++) {
      var edges = cells[c].halfedges;

      ctx.strokeStyle = colors[c].strokeStyle;

      ctx.beginPath();
      // closePath fills in last edge
      for (var e = 0; e < edges.length - 1; e++) {
        var edge = edges[e];
        var startpoint = edge.getStartpoint();
        var endpoint = edge.getEndpoint();

        if (e == 0) {
          ctx.moveTo(startpoint.x, startpoint.y);
        }
        ctx.lineTo(endpoint.x, endpoint.y);
      }
      ctx.closePath();

      ctx.stroke();
    }
  }

  var xSpeed = 10;
  var ySpeed = 10;
  function update(delta) {
    for (var p = 0; p < points.length; p++) {
      var angle = points[p].angle;
      var dx = Math.cos(angle) * xSpeed * delta;
      var dy = Math.sin(angle) * ySpeed * delta;
      points[p].x += dx;
      points[p].y += dy;
      points[p].angle += Math.random() - 0.5;
    }
  }

  function step(timestamp) {
    var delta = previousTimestamp ? (timestamp - previousTimestamp) / 1000 : 0;
    previousTimestamp = timestamp;

    update(delta);

    render();

    if (isPlaying) {
      window.requestAnimationFrame(step);
    }
  }

  function setSizes() {
    mainCanvas.width = window.innerWidth - 20;
    mainCanvas.height = 600;

    bbox = {
      xl: -10,
      xr: mainCanvas.width + 10,
      yt: -10,
      yb: mainCanvas.height + 10
    };

    ctx.lineWidth = 2;
  }

  function setPoints() {
    var N = 50;

    points = [];
    colors = [];
    for (var i = 0; i < N; i++) {
      var x = randInt(0, bbox.xr);
      var y = randInt(0, bbox.yb);
      var angle = 0;
      points.push(point(x, y, angle));

      var stroke = "rgba(0, 0, 0, 0.1)";
      colors.push(color(stroke));
    }
  }

  function play() {
    isPlaying = true;
    playPauseButton.innerHTML = "Pause";
    previousTimestamp = null;
    window.requestAnimationFrame(step);
  }

  function pause() {
    isPlaying = false;
    playPauseButton.innerHTML = "Play";
  }

  function togglePlayPause() {
    if (isPlaying) {
      pause();
    } else {
      play();
    }
  }

  window.onload = function () {
    // --- VORONOI ---

    mainCanvas = document.getElementById("voronoi");
    ctx = mainCanvas.getContext("2d");

    var resizeTimer;
    window.onresize = function() {
      setSizes();

      // Only execute when finished resizing
      clearTimeout(resizeTimer);
      resizeTimer = window.setTimeout(function() {
        setPoints();
      }, 250);
    };

    playPauseButton = document.createElement("div");
    playPauseButton.id = "play-pause";
    playPauseButton.innerHTML = "Pause";
    playPauseButton.onclick = togglePlayPause;

    var mainHeader = document.getElementById("main-header");
    mainHeader.insertBefore(playPauseButton, mainHeader.childNodes[0]);

    // Pause after a bit
    window.setTimeout(togglePlayPause, 30 * 1000);

    setSizes();
    setPoints();

    window.requestAnimationFrame(step);

    // --- STICKY INFO ---
    // Inspired by https://www.w3schools.com/howto/howto_js_sticky_header.asp

    var mainDiv = document.getElementById("main");
    var mainTop = main.offsetTop;
    var info = document.getElementById("info");

    window.onscroll = function() {
      if (window.pageYOffset > mainTop) {
        info.classList.add("sticky");
      } else {
        info.classList.remove("sticky");
      }
    };
  }
})();
