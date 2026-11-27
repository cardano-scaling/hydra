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
  const fundsArea = app.getByRole("region", { name: "Funds", exact: true });
  await expect(fundsArea).toHaveAttribute("aria-disabled", "true");
  await expect(fundsArea.locator("[inert]")).toHaveCSS("opacity", "0.4");
  await expect(
    fundsArea.locator('[role="button"][aria-disabled="false"]')
  ).toHaveCount(0);
  await expect(fundsArea.locator('[role="button"][tabindex="0"]')).toHaveCount(
    0
  );
  await expect(fundsArea.locator('[data-enabled="true"]')).toHaveCount(0);
  const lockedFund = fundsArea.locator('[data-fund="coin-0"]');
  assert.equal(
    await lockedFund.getByRole("button").evaluate((element) => {
      element.focus();
      return document.activeElement === element;
    }),
    false,
    "the funds area cannot receive keyboard focus before opening"
  );
  const lockedBounds = await lockedFund.boundingBox();
  await page.mouse.click(lockedBounds.x + 20, lockedBounds.y + 20);
  await expect(
    app.getByRole("group", { name: "Move selected funds" })
  ).toHaveCount(0);
  await page.screenshot({ path: `${output}/page-desktop.png`, fullPage: true });
  await app.getByLabel("Number of operators").selectOption("3");
  await app.getByLabel("Erin’s connection").selectOption("operator-2");
  await app.getByRole("button", { name: "Open the Head", exact: true }).click();
  await expect(app.getByLabel("Number of operators")).toBeDisabled();
  await expect(fundsArea).toHaveAttribute("data-unavailable", "true");
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
  await expect(fundsArea).toHaveAttribute("aria-disabled", "false");
  await expect(fundsArea.locator("[inert]")).toHaveCount(0);
  const operatorBadges = app.getByLabel("Head operators");
  await expect(operatorBadges.locator('[data-leader="true"]')).toHaveCount(1);
  await expect(
    operatorBadges.locator('[data-operator="operator-0"]')
  ).toContainText("★");
  await expect(operatorBadges.locator('[data-signed="true"]')).toHaveCount(0);
  const source = app.locator('[data-owner="user-0"][data-place="l1"]').first();
  const fundId = await source.getAttribute("data-fund");
  const fund = app.locator(`[data-fund="${fundId}"]`);
  const account = (layer, owner) =>
    app.locator(`[data-account="${layer}-${owner}"]`);
  await expect(account("l2", "operator-0")).toHaveCSS("stroke-width", "2px");
  await expect(account("l1", "operator-0")).toHaveCSS("stroke-width", "1px");
  await expect(account("l2", "user-0")).toHaveCSS("stroke-width", "1px");
  await expect(async () => {
    const colors = await account("l2", "operator-0").evaluate((element) => {
      const sample = document.createElement("span");
      sample.style.color = "var(--ifm-color-primary)";
      document.body.append(sample);
      const colors = [
        getComputedStyle(element).stroke,
        getComputedStyle(sample).color,
      ];
      sample.remove();
      return colors;
    });
    assert.equal(colors[0], colors[1]);
  }).toPass();
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
  const signSnapshot = async (names = ["Alice", "Bob", "Carol"], leader) => {
    await expect(operatorBadges.locator('[data-signed="true"]')).toHaveCount(0);
    if (leader)
      await expect(
        operatorBadges.locator('[data-leader="true"]')
      ).toContainText(leader);
    await app
      .getByRole("button", { name: "Check the snapshot proposal", exact: true })
      .click();
    for (const [index, name] of names.entries()) {
      await app
        .getByRole("button", {
          name: `Deliver ${name}’s signature`,
          exact: true,
        })
        .click();
      await expect(operatorBadges.locator('[data-signed="true"]')).toHaveCount(
        index + 1
      );
      await expect(
        operatorBadges.locator('[data-signed="true"]').filter({ hasText: name })
      ).toContainText("✓");
    }
  };
  await signSnapshot(undefined, "Alice");
  await app.getByRole("button", { name: "Back", exact: true }).click();
  await expect(operatorBadges.locator('[data-signed="true"]')).toHaveCount(2);
  await app.getByRole("button", { name: "Forward", exact: true }).click();
  await expect(operatorBadges.locator('[data-signed="true"]')).toHaveCount(3);
  await app.screenshot({ path: `${output}/signatures-desktop.png` });
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
  await signSnapshot(undefined, "Bob");
  await expect(fund).toHaveAttribute("data-owner", "user-1");
  await expect(
    app.getByText("40 ADA backing the Head", { exact: true })
  ).toBeVisible();
  await drag(fund, account("l1", "user-1"));
  await app
    .getByRole("button", {
      name: "Deliver withdrawal to operators",
      exact: true,
    })
    .click();
  await signSnapshot(undefined, "Carol");
  await expect(fund).toHaveAttribute("data-place", "withdrawal");
  await app.getByRole("button", { name: "Confirm withdrawal on L1" }).click();
  await expect(fund).toHaveAttribute("data-place", "l1");
  await expect(
    app.getByText("305 ADA + 2 DEMO in wallets", { exact: true })
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
    app.getByText("41 ADA confirmed & available", { exact: true })
  ).toBeVisible();
  await expect(app.locator('[data-reserved="true"]')).toHaveCount(0);
  await app.getByRole("button", { name: "Start over", exact: true }).click();
  await expect(app.getByLabel("Number of operators")).toBeEnabled();
  await page.setViewportSize({ width: 1440, height: 1100 });
  await page.evaluate(() =>
    document.documentElement.setAttribute("data-theme", "light")
  );
  await app.getByLabel("Number of operators").selectOption("2");
  await app.getByLabel("Erin’s connection").selectOption("operator-1");
  await app.getByRole("button", { name: "Open the Head", exact: true }).click();
  await app
    .getByRole("button", { name: "Confirm opening on L1", exact: true })
    .click();
  const nativeToken = app.locator('[data-fund="coin-0"]');
  const tokenOnly = app.locator('[data-fund="token-only"]');
  await expect(nativeToken).toHaveAttribute("data-place", "l1");
  await expect(nativeToken.getByRole("button")).toHaveAccessibleName(
    /10 ADA \+ 1 DEMO on L1/
  );
  await expect(tokenOnly.getByRole("button")).toHaveAccessibleName(
    "Alice: 1 DEMO on L1"
  );
  for (const id of ["token-only", "coin-0", "coin-4", "coin-5"]) {
    const output = app.locator(`[data-fund="${id}"]`);
    await expect(output).toHaveAttribute("data-enabled", "true");
    await output.getByRole("button").focus();
    await page.keyboard.press("Enter");
    await app
      .getByRole("button", { name: "Put into the Head", exact: true })
      .click();
    await expect(
      app.getByRole("button", { name: "Close the Head", exact: true })
    ).toBeDisabled();
    await app
      .getByRole("button", { name: "Confirm deposit on L1", exact: true })
      .click();
    if (id === "token-only")
      await expect(app.getByText(/1 DEMO in deposits$/)).toBeVisible();
    await app
      .getByRole("button", { name: "Let the deposit activate", exact: true })
      .click();
    await signSnapshot(["Alice", "Bob"]);
    await app
      .getByRole("button", { name: "Confirm increment on L1", exact: true })
      .click();
    if (id === "token-only") {
      await expect(
        app.getByText("1 DEMO confirmed & available", { exact: true })
      ).toBeVisible();
      await expect(payment).toBeVisible();
    }
  }
  const swap = app.getByRole("form", { name: "Swap token for ADA" });
  await swap.getByLabel("Token output").selectOption("coin-0");
  await swap.getByLabel("Token buyer").selectOption("user-0");
  await expect(swap).toContainText("Receives 61 ADA from");
  await expect(
    swap.getByRole("button", { name: "Agree & swap" })
  ).toBeEnabled();
  await app.screenshot({ path: `${output}/token-swap-desktop.png` });
  await page.setViewportSize({ width: 390, height: 844 });
  await page.evaluate(() =>
    document.documentElement.setAttribute("data-theme", "dark")
  );
  await app.screenshot({ path: `${output}/token-swap-mobile-dark.png` });
  assert.ok(
    await app.evaluate(
      (element) => element.scrollWidth <= element.clientWidth + 1
    )
  );
  await swap.getByRole("button", { name: "Agree & swap" }).click();
  await expect(app.locator('[data-reserved="true"]')).toHaveCount(3);
  await app
    .getByRole("button", { name: "Send transaction to operators", exact: true })
    .click();
  await signSnapshot(["Alice", "Bob"]);
  await expect(nativeToken).toHaveAttribute("data-owner", "user-0");
  await expect(nativeToken).toHaveAttribute("data-place", "l2");
  await expect(
    app.getByText("71 ADA + 2 DEMO backing the Head", { exact: true })
  ).toBeVisible();
  await tokenOnly.getByRole("button").focus();
  await page.keyboard.press("Enter");
  await app.getByRole("button", { name: "Pay Erin", exact: true }).click();
  await app
    .getByRole("button", { name: "Send transaction to operators", exact: true })
    .click();
  await signSnapshot(["Alice", "Bob"]);
  await expect(tokenOnly).toHaveAttribute("data-owner", "user-0");
  await expect(tokenOnly.getByRole("button")).toHaveAccessibleName(
    "Erin: 1 DEMO on L2"
  );
  await app
    .getByRole("button", { name: "Close the Head", exact: true })
    .click();
  await expect(nativeToken).toHaveAttribute("data-place", "l2");
  await app
    .getByRole("button", { name: "Confirm close on L1", exact: true })
    .click();
  await expect(nativeToken).toHaveAttribute("data-place", "closed");
  await expect(nativeToken.getByText("locked", { exact: true })).toBeVisible();
  const fanout = app.getByRole("button", {
    name: "Fan out to L1",
    exact: true,
  });
  await expect(fanout).toBeDisabled();
  await expect(
    app.getByText("71 ADA + 2 DEMO locked until fanout", { exact: true })
  ).toBeVisible();
  await app.screenshot({ path: `${output}/closed-mobile-dark.png` });
  await app
    .getByRole("button", { name: "Wait contestation period", exact: true })
    .click();
  await expect(fanout).toBeEnabled();
  await fanout.click();
  await expect(nativeToken).toHaveAttribute("data-place", "closed");
  await app
    .getByRole("button", { name: "Confirm fanout on L1", exact: true })
    .click();
  await expect(
    app.getByRole("heading", { name: "All funds are back on L1", exact: true })
  ).toBeVisible();
  await expect(nativeToken).toHaveAttribute("data-place", "l1");
  await expect(nativeToken).toHaveAttribute("data-owner", "user-0");
  await expect(
    app.getByText("123 ADA + 2 DEMO in wallets", { exact: true })
  ).toBeVisible();
  await expect(tokenOnly).toHaveAttribute("data-place", "l1");
  await expect(tokenOnly).toHaveAttribute("data-owner", "user-0");
  await app.getByRole("button", { name: "Back", exact: true }).click();
  await expect(nativeToken).toHaveAttribute("data-place", "closed");
  await app.getByRole("button", { name: "Forward", exact: true }).click();
  await expect(nativeToken).toHaveAttribute("data-place", "l1");
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
    `Opening, dragging, deposits, payments, token swaps, withdrawals, close/fanout, keyboard actions, replay, protocol inspection and mobile layout passed. Screenshots: ${output}`
  );
} finally {
  await browser.close();
}
