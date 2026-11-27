import assert from "node:assert/strict";
import { mkdir } from "node:fs/promises";
import { createRequire } from "node:module";
const require = createRequire(import.meta.url);
const { chromium } = require("playwright");
const { expect } = require("@playwright/test");
const url =
  process.argv[2] || "http://localhost:3000/head-protocol/docs/head-playground";
const output = process.argv[3] || "/tmp/hydra-playground-shots";
await mkdir(output, { recursive: true });
const browser = await chromium.launch({ channel: "chromium" });
try {
  const page = await browser.newPage({
    viewport: { width: 1440, height: 1100 },
    hasTouch: true,
  });
  const errors = [];
  page.on("pageerror", (error) => {
    errors.push(error.message);
    console.error(error.message);
  });
  await page.route("**/*", (route) =>
    new URL(route.request().url()).origin === new URL(url).origin
      ? route.continue()
      : route.abort()
  );
  await page.goto(url, { waitUntil: "domcontentloaded" });
  const app = page.getByRole("region", { name: "Hydra Head playground" });
  await expect(app).toBeVisible();
  await expect(
    page.getByRole("navigation", { name: "Docs sidebar" })
  ).toHaveCount(0);
  await expect(app.getByLabel("Number of operators")).toBeVisible();
  await page.screenshot({ path: `${output}/page-desktop.png`, fullPage: true });
  await app.getByLabel("Number of operators").selectOption("3");
  await app.getByLabel("Erin’s connection").selectOption("operator-2");
  await app.getByRole("button", { name: "Open the Head", exact: true }).click();
  await expect(app.getByLabel("Number of operators")).toBeDisabled();
  await expect(app.getByLabel("Number of operators")).toHaveCSS(
    "cursor",
    "not-allowed"
  );
  await app
    .getByRole("button", { name: "Confirm opening on L1", exact: true })
    .click();
  await expect(
    app.getByLabel("Head operators").locator('[data-active="true"]')
  ).toHaveCount(3);
  const source = app.locator('[data-owner="user-0"][data-place="l1"]').first();
  const fundId = await source.getAttribute("data-fund");
  const fund = app.locator(`[data-fund="${fundId}"]`);
  const account = (layer, owner) =>
    app.locator(`[data-account="${layer}-${owner}"]`);
  const drag = async (token, destination) => {
    await expect(app).toHaveAttribute("aria-busy", "false");
    await expect(async () => {
      await token.scrollIntoViewIfNeeded();
      await expect(token).toBeVisible();
    }).toPass({ timeout: 5000 });
    const a = await token.boundingBox(),
      b = await destination.boundingBox();
    await page.mouse.move(a.x + a.width / 2, a.y + a.height / 2);
    await page.mouse.down();
    await page.mouse.move(b.x + b.width / 2, b.y + b.height / 2, { steps: 15 });
    await page.mouse.up();
  };
  await drag(fund, account("l2", "user-0"));
  await expect(
    app.getByRole("heading", { name: "A wallet has submitted a deposit" })
  ).toBeVisible();
  const messages = app.getByRole("region", { name: "Who sends what" });
  await expect(messages.getByRole("listitem")).toHaveCount(1);
  await messages.getByRole("button", { name: /^History/ }).click();
  await expect(messages.getByText("Carol’s node", { exact: true })).toHaveCount(
    2
  );
  await messages
    .getByRole("button", { name: "Hide history", exact: true })
    .click();
  await expect(fund).toHaveAttribute("data-place", "l1");
  await expect(fund).toHaveAttribute("data-reserved", "true");
  await expect(fund.getByRole("button")).toHaveAttribute(
    "aria-disabled",
    "true"
  );
  await expect(fund.getByText("reserved", { exact: true })).toBeVisible();
  await expect(
    app.getByText("Request in progress · funds paused", { exact: true })
  ).toBeVisible();
  await expect(
    app.getByRole("button", { name: "Send payment", exact: true })
  ).toHaveCount(0);
  await app.getByRole("button", { name: "Back", exact: true }).click();
  await expect(fund).toHaveAttribute("data-reserved", "false");
  await expect(fund).toHaveAttribute("data-enabled", "true");
  await app.getByRole("button", { name: "Forward", exact: true }).click();
  await expect(fund).toHaveAttribute("data-reserved", "true");
  await app
    .getByRole("button", { name: "Confirm deposit on L1", exact: true })
    .click();
  await expect(fund).toHaveAttribute("data-place", "deposit");
  await app
    .getByRole("button", { name: "Let the deposit activate", exact: true })
    .click();
  const signSnapshot = async () => {
    await app
      .getByRole("button", { name: "Check the snapshot proposal", exact: true })
      .click();
    for (const name of ["Alice", "Bob", "Carol"]) {
      await app
        .getByRole("button", {
          name: `Deliver ${name}’s signature`,
          exact: true,
        })
        .click();
    }
  };
  await signSnapshot();
  await expect(
    app.getByText("0 ADA confirmed & available", { exact: true })
  ).toBeVisible();
  await app.screenshot({ path: `${output}/deposit-desktop.png` });
  await app.getByRole("button", { name: "Confirm increment on L1" }).click();
  await expect(fund).toHaveAttribute("data-place", "l2");
  const payment = app.getByRole("form", { name: "Send a payment" });
  await payment.getByLabel("Payment recipient").selectOption("user-1");
  await expect(payment).toContainText("Erin’s wallet → Carol’s node · NewTx");
  await payment
    .getByRole("button", { name: "Send payment", exact: true })
    .click();
  await expect(payment.getByLabel("Payment sender")).toBeDisabled();
  await expect(
    app.getByRole("heading", { name: "Share the payment" })
  ).toBeVisible();
  await app
    .getByRole("button", { name: "Send transaction to operators", exact: true })
    .click();
  await signSnapshot();
  await expect(fund).toHaveAttribute("data-owner", "user-1");
  await expect(
    app.getByText("10 ADA backing the Head", { exact: true })
  ).toBeVisible();
  await drag(fund, account("l1", "user-1"));
  await app
    .getByRole("button", {
      name: "Deliver withdrawal to operators",
      exact: true,
    })
    .click();
  await signSnapshot();
  await expect(fund).toHaveAttribute("data-place", "withdrawal");
  await app.getByRole("button", { name: "Confirm withdrawal on L1" }).click();
  await expect(fund).toHaveAttribute("data-place", "l1");
  await expect(
    app.getByText("100 ADA in wallets", { exact: true })
  ).toBeVisible();

  await app.getByRole("button", { name: "Back", exact: true }).click();
  await expect(fund).toHaveAttribute("data-place", "withdrawal");
  await app.getByRole("button", { name: "Forward", exact: true }).click();
  await expect(fund).toHaveAttribute("data-place", "l1");
  await app.getByText("See what the nodes know", { exact: true }).click();
  const aliceState = app
    .locator("summary")
    .filter({ hasText: "Alice’s node · snapshot 3 · Head version 2" });
  await expect(aliceState).toBeVisible();
  await aliceState.click();
  await expect(
    app.getByText("Decisions for ChainInput", { exact: true }).first()
  ).toBeVisible();
  await app.getByText("See what the nodes know", { exact: true }).click();

  const alice = app
    .locator('[data-owner="operator-0"][data-place="l1"]')
    .first();
  await drag(alice, account("l2", "operator-1"));
  await expect(alice).toHaveAttribute("data-place", "l1");
  await expect(
    app.getByRole("heading", { name: "The Head is open", exact: true })
  ).toBeVisible();
  await alice.getByRole("button").focus();
  const cancelFrom = await alice.boundingBox();
  const cancelTo = await account("l2", "operator-0").boundingBox();
  await page.mouse.move(cancelFrom.x + 28, cancelFrom.y + 22);
  await page.mouse.down();
  await page.mouse.move(
    cancelTo.x + cancelTo.width / 2,
    cancelTo.y + cancelTo.height / 2,
    { steps: 10 }
  );
  await page.keyboard.press("Escape");
  await page.mouse.up();
  await expect(alice).toHaveAttribute("data-place", "l1");
  await alice.getByRole("button").focus();
  await page.keyboard.press("Enter");
  await expect(
    app.getByRole("button", { name: "Put into the Head", exact: true })
  ).toBeFocused();
  await app
    .getByRole("button", { name: "Put into the Head", exact: true })
    .click();
  await expect(
    app.getByRole("heading", { name: "A wallet has submitted a deposit" })
  ).toBeVisible();

  await app.getByRole("button", { name: "Start over", exact: true }).click();
  await app.getByLabel("Number of operators").selectOption("1");
  await app.getByLabel("Number of wallet users").selectOption("1");
  await page.setViewportSize({ width: 390, height: 844 });
  await page.emulateMedia({ reducedMotion: "reduce" });
  await app.getByRole("button", { name: "Open the Head", exact: true }).click();
  await app
    .getByRole("button", { name: "Confirm opening on L1", exact: true })
    .click();
  const touchFund = app
    .locator('[data-owner="user-0"][data-place="l1"]')
    .first();
  await expect(app).toHaveAttribute("aria-busy", "false");
  await expect(async () => {
    await touchFund.scrollIntoViewIfNeeded();
    await expect(touchFund).toBeVisible();
  }).toPass({ timeout: 5000 });
  const a = await touchFund.boundingBox(),
    b = await account("l2", "user-0").boundingBox();
  const session = await page.context().newCDPSession(page);
  const start = { x: a.x + a.width / 2, y: a.y + a.height / 2 };
  const end = { x: b.x + b.width / 2, y: b.y + b.height / 2 };
  await session.send("Input.dispatchTouchEvent", {
    type: "touchStart",
    touchPoints: [start],
  });
  for (let step = 1; step <= 10; step++)
    await session.send("Input.dispatchTouchEvent", {
      type: "touchMove",
      touchPoints: [
        {
          x: start.x + ((end.x - start.x) * step) / 10,
          y: start.y + ((end.y - start.y) * step) / 10,
        },
      ],
    });
  await session.send("Input.dispatchTouchEvent", {
    type: "touchEnd",
    touchPoints: [],
  });
  await expect(
    app.getByRole("heading", { name: "A wallet has submitted a deposit" })
  ).toBeVisible();
  assert.ok(
    await app.evaluate(
      (element) => element.scrollWidth <= element.clientWidth + 1
    ),
    "mobile overflow"
  );
  await app.screenshot({ path: `${output}/deposit-mobile.png` });
  await page.evaluate(() =>
    document.documentElement.setAttribute("data-theme", "dark")
  );
  await app.screenshot({ path: `${output}/deposit-mobile-dark.png` });
  const finishDeposit = async () => {
    for (const name of [
      "Confirm deposit on L1",
      "Let the deposit activate",
      "Check the snapshot proposal",
      "Deliver Alice’s signature",
      "Confirm increment on L1",
    ])
      await app.getByRole("button", { name, exact: true }).click();
  };
  await finishDeposit();
  await expect(app).toHaveAttribute("aria-busy", "false");
  const second = app.locator('[data-owner="user-0"][data-place="l1"]').first();
  await expect(second).toHaveAttribute("data-enabled", "true");
  await second.getByRole("button").focus();
  await page.keyboard.press("Enter");
  await app
    .getByRole("button", { name: "Put into the Head", exact: true })
    .click();
  await finishDeposit();
  await payment.getByLabel("Payment amount").selectOption("2");
  await expect(payment).toContainText(
    "2 outputs · Erin’s wallet → Alice’s node · NewTx"
  );
  await app.screenshot({ path: `${output}/payment-mobile-dark.png` });
  await payment
    .getByRole("button", { name: "Send payment", exact: true })
    .click();
  await expect(app.locator('[data-reserved="true"]')).toHaveCount(2);
  for (const name of [
    "Send transaction to operators",
    "Check the snapshot proposal",
    "Deliver Alice’s signature",
  ])
    await app.getByRole("button", { name, exact: true }).click();
  await expect(
    app.locator('[data-owner="operator-0"][data-place="l2"]')
  ).toHaveCount(2);
  await expect(
    app.getByText("20 ADA confirmed & available", { exact: true })
  ).toBeVisible();
  await expect(app.locator('[data-reserved="true"]')).toHaveCount(0);
  await app.getByRole("button", { name: "Start over", exact: true }).click();
  await expect(app.getByLabel("Number of operators")).toBeEnabled();
  await page.setViewportSize({ width: 1440, height: 1100 });
  await page.goto(new URL("./protocol-overview", url).href);
  const sidebar = page.getByRole("navigation", { name: "Docs sidebar" });
  await expect(sidebar).toBeVisible();
  await sidebar
    .getByRole("link", { name: "Try a Hydra Head", exact: true })
    .click();
  await expect(
    page.getByRole("navigation", { name: "Docs sidebar" })
  ).toHaveCount(0);
  assert.deepEqual(errors, [], "browser exceptions");
  console.log(
    `Opening, mouse/touch dragging, deposits, payments, withdrawals, keyboard actions, rewind/replay, protocol inspection, reset and mobile layout passed. Screenshots: ${output}`
  );
} finally {
  await browser.close();
}
