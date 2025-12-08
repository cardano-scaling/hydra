# 🐉 Hydra Lightweight Demo

A web application demonstrating the **lightweight** use of Hydra - no `cardano-node` required! This demo allows you to interact with a Hydra Head using only:

- **Blockfrost API** for L1 (Cardano) interactions
- **hydra-node WebSocket** for L2 (Hydra) interactions
- **Browser-based wallet** for transaction signing

## ✨ Features

| Feature | Description |
|---------|-------------|
| 🔌 **Connect to Hydra Node** | WebSocket connection to your local hydra-node |
| 💰 **Wallet Generation** | Create or restore wallets, export `.sk` and `.vk` files |
| 📥 **Commit UTxOs** | Select L1 UTxOs and commit them to the Head via HTTP API |
| 💸 **Submit Transactions** | Build and sign Cardano transactions inside the Head |
| 📊 **View Snapshots** | Watch confirmed snapshots and UTxO state changes |
| 🔒 **Close & Fanout** | Complete Head lifecycle management |
| 📋 **Event Log** | Real-time event stream from the Hydra node |

## 🚀 Quick Start

### Prerequisites

1. **Node.js 18+** and **pnpm** installed
2. **Blockfrost API key** for Preview network (get one free at [blockfrost.io](https://blockfrost.io))
3. **hydra-node** (built from this repo or via `nix develop`)

### Step 1: Start the Web App

```bash
cd hydra-sdk-app
pnpm install
pnpm dev
```

Open http://localhost:5173 in your browser.

### Step 2: Configure Blockfrost & Create Wallet

1. Enter your **Blockfrost API key** (e.g., `previewXXXXX`) and click **Save**
2. Click **"✨ Generate New Wallet"** to create a new wallet
3. **Save your private key!** It will only be shown once
4. Click **"💾 Export .sk"** and **"📤 Export .vk"** to download your key files

### Step 3: Fund Your Wallet (Required!)

Your wallet needs test ADA to pay for L1 transactions ("fuel").

1. **Copy your address** from the app (starts with `addr_test1...`)

2. **Go to the Cardano Faucet:**
   👉 https://docs.cardano.org/cardano-testnets/tools/faucet

3. **Select "Preview Testnet"** from the Environment dropdown

4. **Paste your address** and complete the CAPTCHA

5. **Click "Request Funds"** - you'll receive ~10,000 test ADA

6. **Wait ~20 seconds** for the transaction to confirm, then click the refresh button (⟳) in the app to see your balance

### Step 4: Generate Hydra Keys

In a terminal, generate a Hydra signing key pair:

```bash
cd /path/to/hydra
cabal run hydra-node -- gen-hydra-key --output-file hydra
```

This creates `hydra.sk` and `hydra.vk` files.

### Step 5: Prepare Files

Make sure you have these files in your hydra repo root:

| File | Description | How to get it |
|------|-------------|---------------|
| `blockfrost-project.txt` | Your Blockfrost API key | `echo "previewXXXXXX" > blockfrost-project.txt` |
| `hydra.sk` | Hydra signing key | `cabal run hydra-node -- gen-hydra-key --output-file hydra` |
| `cardano.sk` | Cardano signing key | Export from web app or create manually |
| `protocol-parameters.json` | Protocol parameters | `cp demo/devnet/protocol-parameters.json .` |

### Step 6: Start Hydra Node

```bash
cd /path/to/hydra

cabal run hydra-node -- \
  --node-id 1 \
  --api-host 0.0.0.0 \
  --api-port 4001 \
  --blockfrost ./blockfrost-project.txt \
  --hydra-signing-key ./hydra.sk \
  --cardano-signing-key ./cardano.sk \
  --hydra-scripts-tx-id "ee449c99464c5419954f39b98e513b17406e24c9883e6342e073006e54878524,d6e03afa86cf1d74011ba234ec32fbd102d4332c3891a49419dae318281bc96a,0b32f7cf144090b3a2d6787cb5b4cabbc0a72c1ae77bf72de8e3d9aa9476bfb7" \
  --ledger-protocol-parameters ./protocol-parameters.json \
  --persistence-dir ./persistence \
  --contestation-period 60s
```

You should see logs like:
```json
{"message":{"node":{"tag":"LoadedState"},"tag":"Node"}}
{"message":{"node":{"tag":"ReplayingState"},"tag":"Node"}}
{"message":{"directChain":{"contents":{"tag":"BeginInitialize"},"tag":"Wallet"},"tag":"DirectChain"}}
```

### Step 7: Connect from Web App

1. In the web app, make sure **WebSocket URL** is `ws://localhost:4001`
2. Click **Connect**
3. You should see "Status: connected" and the Head state

## 📖 Head Lifecycle

| Action | When Available | Description |
|--------|----------------|-------------|
| **Init Head** | Idle | Initialize a new Head |
| **Commit** | Initializing | Commit L1 UTxOs to the Head |
| **Abort** | Initializing | Abort before all commits |
| **New Tx** | Open | Submit a transaction in the Head |
| **Close** | Open | Initiate Head closure |
| **Fanout** | FanoutPossible | Distribute final UTxOs to L1 |

## 🌐 Network Configuration

The app auto-detects your network based on your Blockfrost API key prefix:

| API Key Prefix | Network | Scripts TX ID (v1.1.0) |
|----------------|---------|------------------------|
| `mainnet...` | Mainnet | See `hydra-node/networks.json` |
| `preview...` | Preview | `ee449c99...` (3 IDs) |
| `preprod...` | Preprod | `407bf714...` (3 IDs) |

## 🔧 Troubleshooting

### "NoUTxOFound" error when starting hydra-node
Your `cardano.sk` wallet has no funds. Follow Step 3 to fund it via the faucet.

### "WebSocket connection failed"
The hydra-node isn't running. Start it with the command in Step 6.

### "404 Not Found" when fetching UTxOs
This is normal for new wallets with no funds. Fund your wallet via the faucet.

### "Blockfrost error: 403"
Your API key is for a different network. Make sure your key prefix matches your target network.

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Browser (React App)                      │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐ │
│  │  Wallet     │  │  Blockfrost │  │  Hydra WebSocket   │ │
│  │  (.sk file) │  │  Provider   │  │  Connection        │ │
│  └─────────────┘  └─────────────┘  └─────────────────────┘ │
└──────────────────────────────────────────────────────────────┘
          │                    │                    │
          ▼                    ▼                    ▼
   ┌────────────┐      ┌────────────────┐   ┌────────────────┐
   │ Hydra Node │      │   Blockfrost   │   │   Cardano L1   │
   │  ws:4001   │      │   (preview)    │──▶│  (via BF API)  │
   └────────────┘      └────────────────┘   └────────────────┘
```

## 🔧 Technology Stack

- **React 19** - UI framework
- **TypeScript** - Type safety
- **Vite** - Build tool
- **@hydra-sdk/core** - Wallet management
- **@hydra-sdk/cardano-wasm** - Cardano serialization
- **Blockfrost API** - L1 chain queries and tx submission

## 📁 Key Files

| File | Purpose |
|------|---------|
| `src/App.tsx` | Main application with wallet & Hydra logic |
| `src/hooks/useHydra.ts` | WebSocket connection & Hydra state management |
| `src/components/CommitModal.tsx` | UTxO selection for commit |
| `protocol-parameters.json` | Cardano protocol parameters |

## 🔒 Security Notes

- **Never share your signing keys** - They are only processed locally in the browser
- **Use testnet keys** - This demo is designed for preview/preprod networks
- **API keys are stored in localStorage** - Clear browser data when done

## 📚 References

- [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnets/tools/faucet)
- [Hydra Documentation](https://hydra.family/head-protocol/)
- [Blockfrost API](https://blockfrost.io/docs)
- [Hydra SDK (VTech)](https://github.com/Vtechcom/hydra-sdk)

## License

Apache 2.0
