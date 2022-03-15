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


const CANVAS_SIZE = 32;

const canvas = document.querySelector('canvas');
const ctx = canvas.getContext('2d');
ctx.scale(canvas.width/CANVAS_SIZE, canvas.width/CANVAS_SIZE);

const drawPixel = (x, y, rgb) => {
  const [r,g,b] = rgb;
  ctx.fillStyle = `rgb(${r}, ${g}, ${b})`;
  ctx.fillRect(x, y, 1, 1);
}

drawPixel(15, 15, [255, 0, 0]);
drawPixel(10, 10, [0, 250, 0]);
