# Hydra head with friends

Open a shared [Hydra Head](https://hydra.family) with a group of friends on the
Cardano **preprod** testnet. Everyone runs this same bundle on their own
machine: one `cardano-node` plus one `hydra-node`, in Docker, joined into one
layer-2 head. Free faucet ada, no real funds at risk.

This is a **Claude skill**. The quickest path is to open this folder in Claude
Code and say *"help me set up my Hydra head with my friends"*: Claude reads
`SKILL.md` and walks you through it, filling in the config as you go. The manual
steps are below if you prefer.

## Prerequisites

- Docker (with Compose v2).
- A way for your nodes to reach each other on the peer port. Easiest and safest:
  put the group on a VPN (WireGuard / Tailscale). Otherwise a forwarded public
  port each. Peer traffic is unencrypted, so prefer the VPN.
- A few GB of disk for the chain snapshot.

## Quick start

```bash
cp .env.example .env          # set MY_NAME and ports
./setup.sh                    # fetch config, make keys, print addresses, bootstrap DB
```

Fund the two printed addresses from the preprod faucet, then exchange with each
friend: your public `HOST:PORT` and the two files `peers/<name>-node.vk` and
`peers/<name>-hydra.vk`. Drop theirs into `peers/`.

Edit `config/hydra-node.yaml`: set your `advertise`, point the signing keys at
your own files, and agree on one shared `peers:` block for the whole group
(see the comments in that file). Then:

```bash
docker compose up -d cardano-node                 # wait until fully synced
docker compose exec cardano-node cardano-cli query tip --testnet-magic 1
docker compose up -d hydra-node                   # look for PeerConnected in logs
docker compose --profile tui run --rm hydra-tui   # drive the head: Init, Commit, ...
```

## The important gotchas

- `network: preprod` selects the Hydra scripts only; you still need
  `testnet-magic: 1` for the network magic.
- The `peers:` block, `contestation-period`, `network`, and
  `protocol-parameters.json` must be **identical** for everyone, or the head
  will not open.
- Each `advertise` must be a public address that matches, exactly, what your
  friends list for you.
- Never expose the API port (4001) publicly (it is unauthenticated), and never
  share a `.sk` file.

See `SKILL.md` for the full walkthrough, the config reference, and
troubleshooting.
