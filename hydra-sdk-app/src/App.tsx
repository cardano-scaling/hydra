import React, { useState, useEffect, useCallback } from 'react';
import { ProviderUtils, CardanoCliWallet, NETWORK_ID, KeysUtils } from '@hydra-sdk/core';
import { CardanoWASM } from '@hydra-sdk/cardano-wasm';
import { TxBuilder } from '@hydra-sdk/transaction';
import { Buffer } from 'buffer';
import { useHydra } from './hooks/useHydra';
import {
    Header,
    WalletSetup,
    WalletCard,
    NodeConnection,
    HeadStateCard,
    TabsPanel,
    CommitModal,
    NodeSetupModal,
    Card,
    CardHeader,
    Input,
    Button,
    Badge,
} from './components';
import './index.css';

const { BlockfrostProvider } = ProviderUtils;

interface UTxO {
    txHash: string;
    outputIndex: number;
    address: string;
    amount: { unit: string; quantity: string }[];
}

const App = () => {
    // Configuration State
    const [apiKey, setApiKey] = useState('');
    const [tempApiKey, setTempApiKey] = useState('');
    const [provider, setProvider] = useState<any>(null);
    const [network, setNetwork] = useState<'mainnet' | 'preview' | 'preprod'>('preview');

    // Wallet State
    const [wallet, setWallet] = useState<{ address: string } | null>(null);
    const [skeyHex, setSkeyHex] = useState<`5820${string}` | null>(null);
    const [vkeyHex, setVkeyHex] = useState<`5820${string}` | null>(null);
    const [address, setAddress] = useState<string | null>(null);
    const [mnemonic, setMnemonic] = useState<string | null>(null);
    const [showMnemonic, setShowMnemonic] = useState(false);
    const [mnemonicInput, setMnemonicInput] = useState('');
    const [walletMode, setWalletMode] = useState<'none' | 'file' | 'mnemonic'>('none');
    const [l1Utxos, setL1Utxos] = useState<UTxO[]>([]);
    const [l1Loading, setL1Loading] = useState(false);

    // Hydra Connection
    const [nodeUrl, setNodeUrl] = useState('ws://localhost:4001');
    const [showNodeSetupModal, setShowNodeSetupModal] = useState(false);
    const [showCommitModal, setShowCommitModal] = useState(false);

    // UI State
    const [error, setError] = useState<string | null>(null);

    // Transaction Form
    const [txRecipient, setTxRecipient] = useState('');
    const [txAmount, setTxAmount] = useState('');
    const [txSubmitting, setTxSubmitting] = useState(false);

    // Split UTxO - tracks which UTxO is currently being split (txHash#outputIndex)
    const [splittingUtxo, setSplittingUtxo] = useState<string | null>(null);

    // Hydra Keys State
    const [hydraSkeyHex, setHydraSkeyHex] = useState<string | null>(null);
    const [hydraVkeyHex, setHydraVkeyHex] = useState<string | null>(null);

    // Hydra Hook
    const {
        status,
        headState,
        headId,
        headUtxos,
        snapshots,
        peers,
        logs,
        connectionError,
        connect,
        disconnect,
        clearConnectionError,
        init: initHead,
        abort,
        closeHead,
        fanout,
        sendRawTx,
        clearLogs
    } = useHydra();

    // Load API key from localStorage
    useEffect(() => {
        const savedKey = localStorage.getItem('blockfrost-api-key');
        if (savedKey) {
            const cleanKey = savedKey.trim().replace(/[^\x00-\x7F]/g, '');
            setApiKey(cleanKey);
            setTempApiKey(cleanKey);
        }
    }, []);

    // Create Blockfrost provider
    useEffect(() => {
        const trimmedApiKey = apiKey?.trim();
        if (trimmedApiKey) {
            try {
                let detectedNetwork: 'mainnet' | 'preview' | 'preprod' = 'preview';
                if (trimmedApiKey.startsWith('mainnet')) detectedNetwork = 'mainnet';
                else if (trimmedApiKey.startsWith('preview')) detectedNetwork = 'preview';
                else if (trimmedApiKey.startsWith('preprod')) detectedNetwork = 'preprod';

                setNetwork(detectedNetwork);
                const newProvider = new BlockfrostProvider({
                    apiKey: trimmedApiKey,
                    network: detectedNetwork,
                    apiVersion: 0
                });
                setProvider(newProvider);
                setError(null);
            } catch (e) {
                setError(`Blockfrost init error: ${e instanceof Error ? e.message : String(e)}`);
                setProvider(null);
            }
        } else {
            setProvider(null);
        }
    }, [apiKey]);

    const handleApiKeySave = () => {
        const cleanKey = tempApiKey.trim().replace(/[^\x00-\x7F]/g, '');
        localStorage.setItem('blockfrost-api-key', cleanKey);
        setApiKey(cleanKey);
        setTempApiKey(cleanKey);
    };

    // Query L1 UTxOs using SDK provider.fetcher
    const fetchL1Utxos = useCallback(async () => {
        if (!provider || !address) return;
        setL1Loading(true);
        try {
            // Use SDK's fetcher instead of raw fetch
            const utxos = await provider.fetcher.fetchAddressUTxOs(address);
            // Map SDK UTxO format to our interface
            const mappedUtxos: UTxO[] = utxos.map((u: any) => ({
                txHash: u.input?.txHash || u.txHash,
                outputIndex: u.input?.outputIndex ?? u.outputIndex,
                address: u.output?.address || u.address || address,
                amount: u.output?.amount || u.amount || [{ unit: 'lovelace', quantity: String(u.output?.value?.lovelace || 0) }]
            }));
            setL1Utxos(mappedUtxos);
        } catch (e: any) {
            console.error('Failed to fetch L1 UTxOs:', e);
            // Handle "not found" case (empty address)
            if (e?.message?.includes('404') || e?.status === 404) {
                setL1Utxos([]);
                return;
            }
            setError(`Failed to fetch UTxOs: ${e instanceof Error ? e.message : String(e)}`);
        } finally {
            setL1Loading(false);
        }
    }, [provider, address]);

    useEffect(() => {
        if (wallet && address && provider) {
            fetchL1Utxos();
        }
    }, [wallet, address, provider, fetchL1Utxos]);

    // Generate new wallet
    const handleGenerateWallet = async () => {
        if (!provider) {
            setError("Please save a valid Blockfrost API key first.");
            return;
        }
        try {
            const randomBytes = new Uint8Array(32);
            crypto.getRandomValues(randomBytes);
            const privateKey = CardanoWASM.PrivateKey.from_normal_bytes(randomBytes);
            const publicKey = privateKey.to_public();
            const pkh = publicKey.hash();
            const paymentCred = CardanoWASM.Credential.from_keyhash(pkh);
            const enterpriseAddress = CardanoWASM.EnterpriseAddress.new(0, paymentCred);
            const addressBech32 = enterpriseAddress.to_address().to_bech32();
            const skey = `5820${Buffer.from(privateKey.as_bytes()).toString('hex')}` as `5820${string}`;
            const vkey = `5820${Buffer.from(publicKey.as_bytes()).toString('hex')}` as `5820${string}`;

            setWallet({ address: addressBech32 });
            setSkeyHex(skey);
            setVkeyHex(vkey);
            setAddress(addressBech32);
            setMnemonic(`Private Key (hex): ${skey.slice(4)}`);
            setWalletMode('mnemonic');
            setShowMnemonic(true);
            setError(null);
        } catch (e) {
            setError(`Failed to generate wallet: ${e instanceof Error ? e.message : String(e)}`);
        }
    };

    // Restore wallet from private key
    const handleRestoreWallet = async () => {
        if (!provider) {
            setError("Please save a valid Blockfrost API key first.");
            return;
        }
        const input = mnemonicInput.trim();
        if (/^[0-9a-fA-F]{64}$/.test(input)) {
            try {
                const keyBytes = Buffer.from(input, 'hex');
                const privateKey = CardanoWASM.PrivateKey.from_normal_bytes(keyBytes);
                const publicKey = privateKey.to_public();
                const pkh = publicKey.hash();
                const paymentCred = CardanoWASM.Credential.from_keyhash(pkh);
                const enterpriseAddress = CardanoWASM.EnterpriseAddress.new(0, paymentCred);
                const addressBech32 = enterpriseAddress.to_address().to_bech32();
                const skey = `5820${input}` as `5820${string}`;
                const vkey = `5820${Buffer.from(publicKey.as_bytes()).toString('hex')}` as `5820${string}`;

                setWallet({ address: addressBech32 });
                setSkeyHex(skey);
                setVkeyHex(vkey);
                setAddress(addressBech32);
                setMnemonic(null);
                setWalletMode('mnemonic');
                setMnemonicInput('');
                setError(null);
            } catch (e) {
                setError(`Failed to restore from key: ${e instanceof Error ? e.message : String(e)}`);
            }
        } else {
            setError("Please enter a 64-character hex private key.");
        }
    };

    // Load wallet from .sk file
    const handleFileChange = async (event: React.ChangeEvent<HTMLInputElement>) => {
        const file = event.target.files?.[0];
        if (!file) return;
        setError(null);
        if (!provider) {
            setError("Please save a valid Blockfrost API key first.");
            return;
        }
        try {
            const content = await file.text();
            const keyFile = JSON.parse(content);
            const { cborHex, type } = keyFile;

            if (!cborHex || typeof cborHex !== 'string') {
                throw new Error("Invalid key file: 'cborHex' not found.");
            }
            if (type && !type.includes('PaymentSigningKey') && !type.includes('Signing')) {
                throw new Error(`Wrong key type: ${type}. Expected PaymentSigningKey.`);
            }
            if (!cborHex.startsWith('5820') || cborHex.length !== 68) {
                throw new Error(`Invalid signing key format.`);
            }

            const privateKeyHex = cborHex.slice(4);
            const privateKeyBytes = Buffer.from(privateKeyHex, 'hex');
            const privateKey = CardanoWASM.PrivateKey.from_normal_bytes(privateKeyBytes);
            const publicKey = privateKey.to_public();
            const pkh = publicKey.hash();
            const paymentCred = CardanoWASM.Credential.from_keyhash(pkh);
            const enterpriseAddress = CardanoWASM.EnterpriseAddress.new(0, paymentCred);
            const addressBech32 = enterpriseAddress.to_address().to_bech32();
            const vkey = `5820${Buffer.from(publicKey.as_bytes()).toString('hex')}` as `5820${string}`;

            setWallet({ address: addressBech32 });
            setSkeyHex(cborHex as `5820${string}`);
            setVkeyHex(vkey);
            setAddress(addressBech32);
            setWalletMode('file');
            setMnemonic(null);
        } catch (e) {
            setError(`Failed to load wallet: ${e instanceof Error ? e.message : String(e)}`);
        }
    };

    // Export signing key
    const handleExportSk = () => {
        if (!skeyHex) return;
        const keyFile = {
            type: "PaymentSigningKeyShelley_ed25519",
            description: "Payment Signing Key",
            cborHex: skeyHex
        };
        const blob = new Blob([JSON.stringify(keyFile, null, 4)], { type: 'application/json' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = 'cardano.sk';
        a.click();
        URL.revokeObjectURL(url);
    };

    // Export verification key
    const handleExportVk = () => {
        if (!vkeyHex) return;
        const keyFile = {
            type: "PaymentVerificationKeyShelley_ed25519",
            description: "Payment Verification Key",
            cborHex: vkeyHex
        };
        const blob = new Blob([JSON.stringify(keyFile, null, 4)], { type: 'application/json' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = 'cardano.vk';
        a.click();
        URL.revokeObjectURL(url);
    };

    // Generate Hydra keys using SDK's KeysUtils
    const handleGenerateHydraKeys = () => {
        try {
            const keyPair = KeysUtils.hydraCliKeygen();
            setHydraSkeyHex(keyPair.sk.cborHex);
            setHydraVkeyHex(keyPair.vk.cborHex);
            setError(null);
        } catch (e) {
            setError(`Failed to generate Hydra keys: ${e instanceof Error ? e.message : String(e)}`);
        }
    };

    // Export Hydra signing key
    const handleExportHydraSk = () => {
        if (!hydraSkeyHex) return;
        const keyFile = {
            type: "HydraSigningKey_ed25519",
            description: "Hydra Signing Key",
            cborHex: hydraSkeyHex
        };
        const blob = new Blob([JSON.stringify(keyFile, null, 4)], { type: 'application/json' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = 'hydra.sk';
        a.click();
        URL.revokeObjectURL(url);
    };

    // Export Hydra verification key
    const handleExportHydraVk = () => {
        if (!hydraVkeyHex) return;
        const keyFile = {
            type: "HydraVerificationKey_ed25519",
            description: "Hydra Verification Key",
            cborHex: hydraVkeyHex
        };
        const blob = new Blob([JSON.stringify(keyFile, null, 4)], { type: 'application/json' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = 'hydra.vk';
        a.click();
        URL.revokeObjectURL(url);
    };

    // Disconnect wallet
    const handleDisconnect = () => {
        setWallet(null);
        setSkeyHex(null);
        setVkeyHex(null);
        setAddress(null);
        setMnemonic(null);
        setWalletMode('none');
        setL1Utxos([]);
    };

    // Split a specific UTxO into 2
    const handleSplitUtxo = async (utxo: UTxO) => {
        if (!skeyHex || !vkeyHex || !address || !provider) {
            setError("Wallet not configured");
            return;
        }
        const utxoId = `${utxo.txHash}#${utxo.outputIndex}`;
        setSplittingUtxo(utxoId);
        setError(null);
        try {
            const totalLovelace = parseInt(utxo.amount.find(a => a.unit === 'lovelace')?.quantity || '0');
            const feeReserve = 500_000;
            const availableLovelace = totalLovelace - feeReserve;

            if (availableLovelace < 4_000_000) {
                throw new Error(`Not enough ADA to split (need at least 4.5 ADA)`);
            }

            // Split into 2 equal parts
            const amountPerOutput = Math.floor(availableLovelace / 2);
            const txBuilder = new TxBuilder();
            txBuilder.txIn(
                utxo.txHash,
                utxo.outputIndex,
                utxo.amount.map(a => ({ unit: a.unit, quantity: a.quantity })),
                address
            );
            txBuilder.txOut(address, [{ unit: 'lovelace', quantity: amountPerOutput.toString() }]);
            txBuilder.txOut(address, [{ unit: 'lovelace', quantity: amountPerOutput.toString() }]);
            txBuilder.changeAddress(address);
            const unsignedTx = await txBuilder.complete();
            const unsignedTxHex = Buffer.from(unsignedTx.to_bytes()).toString('hex');
            const networkId = network === 'mainnet' ? NETWORK_ID.MAINNET : NETWORK_ID.PREPROD;
            const cliWallet = new CardanoCliWallet({
                networkId,
                skey: skeyHex,
                vkey: vkeyHex,
                fetcher: provider.fetcher,
                submitter: provider.submitter
            });
            const signedTxHex = await cliWallet.signTx(unsignedTxHex, false);

            // Use SDK's submitter instead of raw fetch
            const txId = await provider.submitter.submitTx(signedTxHex);
            alert(`✅ Split transaction submitted!\nTx ID: ${txId}\n\nWait ~20 seconds for confirmation.`);
            setTimeout(fetchL1Utxos, 20000);
        } catch (e) {
            setError(`Split failed: ${e instanceof Error ? e.message : String(e)}`);
        } finally {
            setSplittingUtxo(null);
        }
    };

    // Handle commit
    const handleCommit = async (selectedUtxos: UTxO[]) => {
        if (!skeyHex || !vkeyHex || !provider || selectedUtxos.length === 0) return;
        try {
            const utxoMap: Record<string, any> = {};
            for (const utxo of selectedUtxos) {
                const txIn = `${utxo.txHash}#${utxo.outputIndex}`;
                utxoMap[txIn] = {
                    address: utxo.address,
                    value: { lovelace: parseInt(utxo.amount.find(a => a.unit === 'lovelace')?.quantity || '0') }
                };
            }

            const response = await fetch('/hydra-api/commit', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(utxoMap)
            });
            if (!response.ok) {
                const errorText = await response.text();
                throw new Error(`Commit request failed: ${errorText}`);
            }
            const commitResponse = await response.json();
            if (!commitResponse?.cborHex) {
                throw new Error('Failed to get commit draft transaction');
            }

            const networkId = network === 'mainnet' ? NETWORK_ID.MAINNET : NETWORK_ID.PREPROD;
            const cliWallet = new CardanoCliWallet({
                networkId,
                skey: skeyHex,
                vkey: vkeyHex,
                fetcher: provider.fetcher,
                submitter: provider.submitter
            });
            const signedTxHex = await cliWallet.signTx(commitResponse.cborHex, true);

            // Use SDK's submitter instead of raw fetch
            const txId = await provider.submitter.submitTx(signedTxHex);
            setError(`✅ Commit tx submitted: ${txId}. Wait ~30s for L1 confirmation...`);
            setShowCommitModal(false);
            setTimeout(fetchL1Utxos, 5000);
            setTimeout(fetchL1Utxos, 15000);
            setTimeout(fetchL1Utxos, 30000);
        } catch (e) {
            setError(`Commit failed: ${e instanceof Error ? e.message : String(e)}`);
        }
    };

    // Handle NewTx submission in Hydra Head
    // Note: In-head transactions have fee=0, so we use CardanoWASM directly
    // TxBuilder would calculate fees which isn't appropriate for Hydra head txs
    const handleNewTx = async () => {
        if (!txRecipient || !txAmount || !skeyHex || !vkeyHex || !provider || headState !== 'Open') return;
        setTxSubmitting(true);
        try {
            const myUtxo = Object.entries(headUtxos).find(([_, output]: [string, any]) => output.address === address);
            if (!myUtxo) throw new Error('No UTxOs found in head for this wallet');

            const [txIn, txOut] = myUtxo;
            const [inputTxHash, inputIndex] = txIn.split('#');
            const inputLovelace = parseInt(txOut.value?.lovelace || txOut.value || '0');
            const sendAmount = parseInt(txAmount);
            if (sendAmount >= inputLovelace) throw new Error('Amount exceeds available balance');

            const changeAmount = inputLovelace - sendAmount;

            // Build transaction using CardanoWASM (fee=0 for Hydra head transactions)
            const inputTxId = CardanoWASM.TransactionHash.from_hex(inputTxHash);
            const input = CardanoWASM.TransactionInput.new(inputTxId, parseInt(inputIndex));
            const inputs = CardanoWASM.TransactionInputs.new();
            inputs.add(input);

            const outputs = CardanoWASM.TransactionOutputs.new();
            const recipientAddr = CardanoWASM.Address.from_bech32(txRecipient);
            outputs.add(CardanoWASM.TransactionOutput.new(
                recipientAddr,
                CardanoWASM.Value.new(CardanoWASM.BigNum.from_str(sendAmount.toString()))
            ));
            if (changeAmount > 0) {
                const changeAddr = CardanoWASM.Address.from_bech32(address!);
                outputs.add(CardanoWASM.TransactionOutput.new(
                    changeAddr,
                    CardanoWASM.Value.new(CardanoWASM.BigNum.from_str(changeAmount.toString()))
                ));
            }

            // Fee = 0 for Hydra in-head transactions
            const txBody = CardanoWASM.TransactionBody.new(inputs, outputs, CardanoWASM.BigNum.from_str('0'), undefined);
            const unsignedTx = CardanoWASM.Transaction.new(txBody, CardanoWASM.TransactionWitnessSet.new(), undefined);
            const unsignedTxHex = Buffer.from(unsignedTx.to_bytes()).toString('hex');

            // Sign using SDK's CardanoCliWallet
            const networkId = network === 'mainnet' ? NETWORK_ID.MAINNET : NETWORK_ID.PREPROD;
            const cliWallet = new CardanoCliWallet({
                networkId,
                skey: skeyHex,
                vkey: vkeyHex,
                fetcher: provider.fetcher,
                submitter: provider.submitter
            });
            const signedTxHex = await cliWallet.signTx(unsignedTxHex, false);

            // Submit to Hydra head via SDK's HydraBridge
            sendRawTx(signedTxHex);
            setTxRecipient('');
            setTxAmount('');
        } catch (e) {
            setError(`Transaction failed: ${e instanceof Error ? e.message : String(e)}`);
        } finally {
            setTxSubmitting(false);
        }
    };

    // Calculate head balance
    const headBalance = Object.entries(headUtxos)
        .filter(([_, output]: [string, any]) => output.address === address)
        .reduce((sum, [_, output]: [string, any]) => {
            const lovelace = output.value?.lovelace || output.value || 0;
            return sum + parseInt(lovelace);
        }, 0);

    return (
        <div className="max-w-7xl mx-auto px-4 py-5">
            <Header />

            {/* Error Banners */}
            {error && (
                <div className="bg-danger/10 border border-danger text-danger px-5 py-3 rounded-lg mb-5 text-center">
                    {error}
                </div>
            )}

            {/* Connection Error Banner */}
            {connectionError && (
                <div className="bg-warning/10 border border-warning text-warning px-5 py-3 rounded-lg mb-5 flex items-center justify-between">
                    <div className="flex items-center gap-3">
                        <span className="text-xl">⚠️</span>
                        <div>
                            <div className="font-medium">Connection Failed</div>
                            <div className="text-sm opacity-80">{connectionError}</div>
                        </div>
                    </div>
                    <button
                        onClick={clearConnectionError}
                        className="text-warning hover:text-warning/80 text-xl px-2"
                    >
                        ×
                    </button>
                </div>
            )}

            {/* Main Layout */}
            <div className="grid grid-cols-1 lg:grid-cols-[320px_1fr] gap-5 items-start">
                {/* Sidebar */}
                <aside className="flex flex-col gap-4">
                    {/* Configuration Card */}
                    <Card>
                        <CardHeader>⚙️ Configuration</CardHeader>

                        <div className="mb-4">
                            <label className="text-sm text-slate-400 mb-1.5 block">Blockfrost API Key</label>
                            <div className="flex gap-2">
                                <Input
                                    type="password"
                                    value={tempApiKey}
                                    onChange={(e) => setTempApiKey(e.target.value)}
                                    placeholder="preprod..."
                                />
                                <Button onClick={handleApiKeySave}>Save</Button>
                            </div>
                            {provider && <Badge variant="success" className="mt-2">✓ {network}</Badge>}
                        </div>

                        {provider && !wallet && (
                            <WalletSetup
                                mnemonicInput={mnemonicInput}
                                onMnemonicInputChange={setMnemonicInput}
                                onGenerateWallet={handleGenerateWallet}
                                onRestoreWallet={handleRestoreWallet}
                                onFileChange={handleFileChange}
                            />
                        )}

                        {wallet && address && (
                            <WalletCard
                                address={address}
                                walletMode={walletMode}
                                l1Utxos={l1Utxos}
                                l1Loading={l1Loading}
                                mnemonic={mnemonic}
                                showMnemonic={showMnemonic}
                                splittingUtxo={splittingUtxo}
                                hydraKeysGenerated={!!(hydraSkeyHex && hydraVkeyHex)}
                                onRefresh={fetchL1Utxos}
                                onExportSk={handleExportSk}
                                onExportVk={handleExportVk}
                                onDisconnect={handleDisconnect}
                                onSplitUtxo={handleSplitUtxo}
                                onHideMnemonic={() => setShowMnemonic(false)}
                                onShowNodeSetup={() => setShowNodeSetupModal(true)}
                                onGenerateHydraKeys={handleGenerateHydraKeys}
                                onExportHydraSk={handleExportHydraSk}
                                onExportHydraVk={handleExportHydraVk}
                            />
                        )}
                    </Card>

                    {/* Node Connection Card */}
                    {wallet && (
                        <NodeConnection
                            nodeUrl={nodeUrl}
                            status={status}
                            onUrlChange={setNodeUrl}
                            onConnect={() => connect(nodeUrl)}
                            onDisconnect={disconnect}
                        />
                    )}
                </aside>

                {/* Main Content */}
                <main className="flex flex-col gap-4">
                    {status !== 'connected' ? (
                        <div className="text-center py-16 px-10 bg-dark-card border border-dashed border-border rounded-xl">
                            <h3 className="text-slate-200 text-xl mb-4">Connect to a Hydra Node</h3>
                            <p className="text-slate-400 mb-2">Load your wallet and connect to start interacting with the Hydra Head.</p>
                            <p className="text-slate-500 text-sm">Make sure hydra-nodes are running locally on ports 4001/4002/4003</p>
                        </div>
                    ) : (
                        <>
                            <HeadStateCard
                                headState={headState}
                                headId={headId}
                                headBalance={headBalance}
                                peers={peers}
                                txRecipient={txRecipient}
                                txAmount={txAmount}
                                txSubmitting={txSubmitting}
                                onInit={initHead}
                                onCommit={() => setShowCommitModal(true)}
                                onAbort={abort}
                                onClose={closeHead}
                                onFanout={fanout}
                                onTxRecipientChange={setTxRecipient}
                                onTxAmountChange={setTxAmount}
                                onSubmitTx={handleNewTx}
                            />

                            <TabsPanel
                                headUtxos={headUtxos}
                                snapshots={snapshots}
                                logs={logs}
                                address={address}
                                onClearLogs={clearLogs}
                            />
                        </>
                    )}
                </main>
            </div>

            {/* Modals */}
            <CommitModal
                isOpen={showCommitModal}
                l1Utxos={l1Utxos}
                onClose={() => setShowCommitModal(false)}
                onCommit={handleCommit}
                onRefresh={fetchL1Utxos}
                loading={l1Loading}
            />

            <NodeSetupModal
                isOpen={showNodeSetupModal}
                onClose={() => setShowNodeSetupModal(false)}
            />
        </div>
    );
};

export default App;
