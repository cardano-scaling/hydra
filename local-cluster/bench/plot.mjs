import chartjs from 'chartjs-node-canvas';
import fs from 'fs';
import assert from 'assert';

// main

const width = 1920;
const height = 1080;
const chartJSNodeCanvas = new chartjs.ChartJSNodeCanvas({ width, height });

const inputFile = process.argv[2];
assert(typeof inputFile === 'string', 'Expected "results.csv" input filepath as 1st argument');

const txs = readCsvFileSync(inputFile);
const svgFile = inputFile.replace("csv", "svg");
fs.writeFileSync(svgFile, await confirmationTimes(inputFile, txs));
console.log("Created plot:", svgFile);

// functions

function readCsvFileSync(filepath) {
  return fs.readFileSync(filepath)
    .toString()
    .split("\n")
    .slice(1)
    .map(s => s.split(","));
}

async function confirmationTimes(name, txs) {
  const data = txs.map((entry, index) => {
    // Parse scientific notation into 'Number'
    const t1 = entry[0];
    const t2 = entry[1];
    if (t1 != undefined && t2 != undefined) {
      const startTime = JSON.parse(t1);
      const confTime = JSON.parse(t2);
      return { x: index, y: confTime };
    }
  });
  const configuration = {
    type: "scatter",
    options: {
      scales: {
        xAxes: [{ scaleLabel: { display: true, labelString: "Transaction number" }}],
        yAxes: [{ scaleLabel: { display: true, labelString: "Confirmation time (s)"}}]
      }
    },
    data: {
      // labels,
      datasets: [{
        label: name,
        fill: true,
        backgroundColor: "#f8c291",
        data
      }],
    },
  };

  return chartJSNodeCanvas.renderToBuffer(configuration);
}
