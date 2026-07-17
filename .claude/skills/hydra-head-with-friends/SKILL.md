---
name: hydra-head-with-friends
description: Set up a Hydra Head between friends using Docker. Each participant runs one cardano-node plus one hydra-node (configured via a YAML file) on the Cardano preprod testnet, and they join into one shared layer-2 head. Use when someone wants to open a Hydra head with other people, run a hydra-node cluster, configure hydra-node peering, or bring up hydra-node + cardano-node with docker-compose.
allowed-tools: Bash, Read, Edit, Write, WebFetch
---

# Open a Hydra head with your friends (Docker, preprod)

Guide the user through standing up their own node and joining a shared Hydra
Head with a group of friends. Everyone runs the **same bundle** on their own
machine; only a few personal values differ. This skill drives one person's
setup. Work through the steps in order and do not skip the shared-value checks
in [Non-negotiable rules](#non-negotiable-rules), which are the usual reason a
head fails to open.

## The mental model

- A Hydra Head is a layer-2 channel between a fixed set of participants. Each
  participant runs **one `hydra-node`** (layer 2) wired to **one `cardano-node`**
  (layer 1, here the public **preprod** testnet).
- The `hydra-node`s talk to each other directly over a peer-to-peer port. The
  networking layer (etcd) is embedded in the `hydra-node` image, so there is
  nothing extra to install.
- Funds move L1 -> head (commit/deposit), around inside the head as ordinary
  Cardano transactions (instant, free), then head -> L1 (close + fanout).
- Preprod uses free faucet ada, so nothing here risks real money.

## What is in this bundle

| File | Role |
|------|------|
| `docker-compose.yaml` | One participant's stack: `cardano-node` + `hydra-node` (+ optional `hydra-tui`). |
| `config/hydra-node.yaml` | The hydra-node configuration. **This is the file to teach the user.** |
| `config/protocol-parameters.json` | The head's ledger rules (fees zeroed). Shared, identical for all. |
| `.env.example` | Versions and your name/ports. Copy to `.env`. |
| `setup.sh` | One-time: fetch preprod config, generate keys, print addresses, Mithril-bootstrap the DB. |

Directories `setup.sh` creates: `cardano-conf/` (L1 config), `credentials/`
(your private keys), `peers/` (everyone's public keys), `data/` (chain DB +
socket), `persistence/` (head state).

## Shared vs personal values

Get this right and the head opens; get it wrong and it silently will not.

- **Personal** (each friend sets their own): `node-id`, `advertise`,
  `hydra-signing-key`, `chain.cardano-signing-key`, and `MY_NAME` in `.env`.
- **Shared, byte-identical across everyone**: the whole `peers:` block,
  `ledger-protocol-parameters`, `chain.network`, and `chain.contestation-period`.

## Steps

Confirm prerequisites first: Docker with Compose v2; this machine reachable by
the friends on the peer port (a forwarded public port, or everyone on a shared
VPN such as WireGuard/Tailscale, which is what I recommend since peer traffic is
unencrypted); and willingness to pull a few GB Mithril snapshot.

**1. Identity.** `cp .env.example .env` and set `MY_NAME` (a short handle like
`alice`), `PEER_PORT`, and confirm the pinned versions. Set `node-id` in
`config/hydra-node.yaml` to the same handle.

**2. Bootstrap.** `chmod +x setup.sh && ./setup.sh`. This fetches the preprod
node config, generates three key pairs (`<name>-node`, `<name>-funds`,
`<name>-hydra`), copies your two public keys into `peers/`, prints the two
addresses to fund, and downloads the Mithril chain snapshot. It is safe to
re-run.

**3. Fund.** Send the printed **node** address at least ~30 tADA (fee fuel) and
the **funds** address whatever you want to transact with, from the preprod
faucet (https://docs.cardano.org/cardano-testnets/tools/faucet/, network =
Pre-Production). One person can fund everyone and forward ada if easier.

**4. Exchange and fill in peers.** Everyone shares, out of band:
   - their public endpoint `HOST:PORT` (their `advertise`), and
   - their two files `peers/<name>-node.vk` and `peers/<name>-hydra.vk`.

   Drop each friend's two `.vk` files into `peers/`. Then edit
   `config/hydra-node.yaml`:
   - set `advertise` to **your** public `HOST:PORT`;
   - point `hydra-signing-key` and `chain.cardano-signing-key` at your own files;
   - replace the `peers:` block with one entry per participant (including
     yourself), each `address` equal to that person's `advertise`, keys named
     `/peers/<name>-{hydra,node}.vk`.

   The finished `peers:` block, `network`, `contestation-period`, and
   `protocol-parameters.json` must be identical for everyone. Have the group
   agree on one `peers:` block and paste it verbatim; only the top-of-file
   personal fields then differ. Adjust the number of entries to the group size
   (a head needs at least two participants).

**5. Start layer 1 and wait for sync.**
```bash
docker compose up -d cardano-node
# watch until syncProgress reaches 100.00 (Mithril gets you most of the way):
watch -n5 'docker compose exec -T cardano-node cardano-cli query tip --testnet-magic 1'
```
Do not start the hydra-node until the cardano-node is fully synced.

**6. Start layer 2 and verify peers.**
```bash
docker compose up -d hydra-node
docker compose logs -f hydra-node        # look for PeerConnected + Greetings
```
Or connect a client: `websocat ws://127.0.0.1:4001 | jq`. You should see a
`PeerConnected` for each friend and a `Greetings` with your own key. If peers
never connect, see [Troubleshooting](#troubleshooting).

**7. Open and use the head.** Any participant can drive it. Easiest is the TUI:
```bash
docker compose --profile tui run --rm hydra-tui
```
From the TUI (or the API) the lifecycle is:
   - **Init**: opens the head (empty). API: send `{"tag":"Init"}` on the websocket.
   - **Commit**: bring funds in. Query a UTxO of your `funds` address, `POST` it
     to `http://127.0.0.1:4001/commit`, then sign the returned tx with your
     `funds.sk` and submit it on L1. Each person commits independently.
   - **Transact**: build a normal Cardano tx spending an in-head UTxO and send
     `{"tag":"NewTx","transaction": <signed-tx>}`. With zeroed fees use `--fee 0`.
   - **Close** then **Fanout**: `{"tag":"Close"}`, wait for `ReadyToFanout`
     after the contestation deadline, then `{"tag":"Fanout"}` to settle back to L1.

Point the user at the full API walkthrough in the tutorial (see
[References](#references)) for exact commands.

## Non-negotiable rules

Enforce these while helping; they are the difference between a head that works
and one that silently does not.

1. **`network: preprod` does NOT set the network magic.** You must also set
   `backend.testnet-magic: 1`. Without it the node silently talks to magic 42
   (nothing). preprod = 1, preview = 2.
2. **The `peers:` block, `contestation-period`, `network`, and
   `protocol-parameters.json` must be identical for every participant.** If they
   differ, the `Init` transaction is ignored and the head never opens.
3. **`advertise` must be your public, routable `HOST:PORT`, and every friend's
   peer entry for you must match it exactly** (same host string, same port). A
   mismatch means the etcd cluster will not form (you will see cluster-id
   errors) and no `PeerConnected`.
4. **Expose only the peer port** (default 5001) to your friends. Never expose
   the API port (4001) to the internet: it is unauthenticated, so anyone who
   reaches it can close the head. `docker-compose.yaml` already binds the API to
   host loopback only.
5. **Peer traffic is not encrypted.** Over the open internet, run the group on a
   VPN (WireGuard/Tailscale) rather than raw forwarded ports.
6. **Availability.** A head only progresses while a majority of nodes are
   connected, and a participant must not be offline longer than half the
   contestation period or it can fall out of sync and be unable to contest.
7. **Never reuse the demo keys** shipped in the Hydra repo, and never send anyone
   a `.sk` file. Only `.vk` files are exchanged.

## Adapting

- **Group size**: add or remove `peers:` entries (and exchange the matching
  `.vk` files). Minimum two participants.
- **Preview instead of preprod**: set `network: preview`, `testnet-magic: 2`,
  fetch config from `.../environments/preview/`, and use the preview Mithril
  aggregator/vkeys. Preview settles faster.
- **Mainnet**: possible but real funds are at risk; use `mainnet: true` (no
  `testnet-magic`), a `contestation-period` >= 43200, and read the Hydra
  "known issues" page first. Do not default anyone to mainnet.
- **Faster head cycles**: lower `contestation-period` (all must match) so
  Close -> Fanout waits are short. Keep it well below your uptime tolerance.

## Troubleshooting

| Symptom | Likely cause / fix |
|---------|--------------------|
| No `PeerConnected`; cluster-id / etcd errors | A peer `address` does not exactly equal that node's `advertise`, or the peer port is not reachable (firewall/NAT). Confirm each side, test with `nc -vz HOST PORT`. |
| `Init` sent but head never opens | A shared value differs. Diff everyone's `peers:` block, `contestation-period`, `network`, and `protocol-parameters.json`. Compare via `curl -s http://127.0.0.1:4001/config`. |
| hydra-node cannot reach the node socket | cardano-node not up/synced yet, or `data/ipc` not shared. Start cardano-node first; check `docker compose exec cardano-node cardano-cli query tip --testnet-magic 1`. |
| Node stuck "catching up" / rejecting input | It fell behind by more than half the contestation period. Let the cardano-node finish syncing; keep the node online. |
| `Close` seems to do nothing | Known issue: resubmit `{"tag":"Close"}`; it can take a couple of tries. |
| Mithril download failed / DB not at `data/db` | Aggregator may be briefly down. Re-run `setup.sh`; if it stays down, sync from genesis (slow) by starting cardano-node with an empty `data/db`. |
| Cardano-node will not sync (era error) | `CARDANO_NODE_VERSION` is too old for current preprod. Bump it in `.env` to a recent release. |

## References

Hydra docs (in the hydra repo under `docs/docs/`, and at https://hydra.family):
- `configuration.md` (the full YAML config reference: keys, peers, self-filter,
  reference scripts, contestation period, `GET /config`).
- `tutorial/index.md` ("Open a head on testnet": the end-to-end API walkthrough
  this skill's step 7 abbreviates).
- Published reference-script tx ids per network/version: `hydra-node/networks.json`.
- Cardano preprod config: https://book.play.dev.cardano.org/environments/
