const client = new WebSocket("ws://13.38.35.167:4001");

client.addEventListener("open", () => {
  console.log("SOCKET OPEN");
});

client.addEventListener("message", e => {
  const msg = JSON.parse(e.data);
  switch (msg.tag) {
    case "TxSeen":
      if (msg.transaction.auxiliaryData != null) {

      }
  }
});


const canvas = document.querySelector('canvas');
const ctx = canvas.getContext('2d');

const drawPixel = (x, y, rgb) => {
  const [r,g,b] = rgb;
  ctx.fillStyle = `rgb(${r}, ${g}, ${b})`;
  ctx.fillRect(x, y, 1, 1);
}

drawPixel(15, 15, [255, 0, 0]);
