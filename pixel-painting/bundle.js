const client = new WebSocket("ws://13.38.35.167:4001");

const metadataLabel = 14;

client.addEventListener("message", e => {
  const msg = JSON.parse(e.data);
  switch (msg.tag) {
    case "TxSeen":
      console.log("New transaction seen", msg.transaction.id);
      if (msg.transaction.auxiliaryData != null) {
        console.log("Transaction has auxiliary data", msg.transaction.auxiliaryData);
        const [x, y, r, g, b] = cbor.decode(msg.transaction.auxiliaryData).get(metadataLabel);
        drawPixel(x, y, [r, g, b]);
      }
    default:
      console.log("Irrelevant message", msg);
  }
});

// Canvas

const CANVAS_SIZE = 32;

const canvas = document.querySelector('canvas');
const ctx = canvas.getContext('2d');
const canvasScale = {
  x: canvas.width/CANVAS_SIZE,
  y: canvas.height/CANVAS_SIZE,
}
ctx.scale(canvasScale.x, canvasScale.y);

const drawPixel = (x, y, rgb) => {
  const [r,g,b] = rgb;
  ctx.fillStyle = `rgb(${r}, ${g}, ${b})`;
  ctx.fillRect(x, y, 1, 1);
}

drawPixel(15, 15, [255, 0, 0]);
drawPixel(10, 10, [0, 250, 0]);

console.log(canvas)
const canvasPosition = {
  x: canvas.offsetLeft,
  y: canvas.offsetTop
};

canvas.addEventListener('click', function(e) {
  const clickedPixel = {
    x: (e.pageX - canvasPosition.x) / canvasScale.x,
    y: (e.pageY - canvasPosition.y) / canvasScale.y
  }
  console.log(clickedPixel, currentColor);
  drawPixel(Math.floor(clickedPixel.x), Math.floor(clickedPixel.y), currentColor);
});

// Color picker

let currentColor = [255,0,0];
const currentColorElement = document.querySelector('#current-color');
const picker = new Picker(currentColorElement);

currentColorElement.style.background = `rgb(${currentColor[0]}, ${currentColor[1]}, ${currentColor[2]})`;

picker.onDone = function(color) {
  console.log("onDone", color);
  currentColor = color.rgba;
  currentColorElement.style.background = color.rgbaString;
};
