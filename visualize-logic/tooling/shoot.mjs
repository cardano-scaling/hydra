// Screenshot the hydra-vis web UI with Playwright.
//
// The UI is a jsaddle-warp app: the served HTML is an empty shell and the DOM
// is built over a websocket, so a one-shot `chromium --screenshot` captures a
// blank page. Playwright holds the page open, waits for the real DOM, and can
// drive the slider, so we get faithful frames to iterate on the design.
//
// Run inside `nix develop .#ui`:
//   node visualize-logic/tooling/shoot.mjs [url] [outDir]
//
// Defaults: url=http://localhost:8092/  outDir=/tmp/hydra-vis-shots
//
// Captures: full page, plus a tight crop of the "state transitions" SVG at a
// few slider positions so arrow placement is easy to eyeball across states.

import { mkdir } from 'node:fs/promises';
import { createRequire } from 'node:module';

// ESM `import` ignores NODE_PATH; `require` honors it, and the devShell points
// NODE_PATH at the nix-provided playwright. Resolve through createRequire so
// the script needs no local node_modules.
const require = createRequire(import.meta.url);
const { chromium } = require('playwright');

const url = process.argv[2] ?? 'http://localhost:8092/';
const outDir = process.argv[3] ?? '/tmp/hydra-vis-shots';

await mkdir(outDir, { recursive: true });

const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1100, height: 1400 } });

await page.goto(url, { waitUntil: 'domcontentloaded' });

// Wait for the websocket-driven DOM: the flow chart SVG appears only after
// jsaddle has run the Haskell view and patched the DOM.
const svg = page.locator('svg').first();
await svg.waitFor({ state: 'visible', timeout: 30_000 });
// Let any follow-up patches settle.
await page.waitForTimeout(500);

await page.screenshot({ path: `${outDir}/full.png`, fullPage: true });
await svg.screenshot({ path: `${outDir}/flow.png` });

// Scrub the slider across a few positions and capture the flow chart at each,
// so we can see which arrow lights up and confirm endpoints stay on the nodes.
const slider = page.locator('input[type=range]').first();
if (await slider.count()) {
  const max = Number(await slider.getAttribute('max')) || 0;
  const stops = [...new Set([0, Math.floor(max / 2), max])];
  for (const v of stops) {
    await slider.evaluate((el, val) => {
      el.value = String(val);
      el.dispatchEvent(new Event('input', { bubbles: true }));
      el.dispatchEvent(new Event('change', { bubbles: true }));
    }, v);
    await page.waitForTimeout(300);
    await svg.screenshot({ path: `${outDir}/flow-at-${v}.png` });
  }
}

await browser.close();
console.log(`wrote screenshots to ${outDir}`);
