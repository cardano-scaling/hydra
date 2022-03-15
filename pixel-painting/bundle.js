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
