(function() {

"use strict";

// Core Audio

let buffers = {};

async function getMusic(path) {
  const response = await fetch(path);
  const arrayBuffer = await response.arrayBuffer();
  const audioBuffer = await audioCtx.decodeAudioData(arrayBuffer);
  return {
    path: path,
    buffer: audioBuffer
  };
}

function play(path) {
  console.log(buffers);
  console.log(path);
  if (buffers[path]) {
    const source = audioCtx.createBufferSource();
    source.buffer = buffers[path];
    source.connect(audioCtx.destination);
    source.start(0, 0);
  }
}

// Message Handling

function init(allSfx) {
  Promise.all(allSfx.map(getMusic)).then(musics => {
    musics.forEach(music => {
      buffers[music.path] = music.buffer;
      console.log("done: " + music.path);
    });
  });
}

function playSfx(sfx) {
  play(sfx);
}

app.ports.sfxToJs.subscribe(function(data) {
  const name = data.name;
  const args = data.args;

  console.log("sfxToJs received: " + name);

  switch (name) {
    case "init":
      init(args.allSfx);
      break;
    case "play":
      playSfx(args.sfx);
      break;
    default:
      console.warn("sfxToJs subscription: unknown command");
      break;
  }
});

})();
