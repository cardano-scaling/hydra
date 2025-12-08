import { useState, useCallback, useEffect, useRef } from 'react';
import { HydraBridge } from '@hydra-sdk/bridge';
import type { UTxO } from '@hydra-sdk/core';

type ConnectionStatus = 'disconnected' | 'connecting' | 'connected';
type HeadState = 'Idle' | 'Initializing' | 'Open' | 'Closed' | 'FanoutPossible' | 'Final';

export interface Peer {
    vkey: string;
    participant?: string;  // Address hash from participants array
    connected: boolean;
    isYou: boolean;
}

export interface Snapshot {
    number: number;
    utxo: Record<string, any>;
    confirmedAt: string;
}

export interface LogEntry {
    timestamp: string;
    type: 'info' | 'success' | 'warning' | 'error' | 'event' | 'tx';
    tag?: string;
    message: string;
    details?: string;
}

// Load logs from localStorage
const loadLogs = (): LogEntry[] => {
    try {
        const stored = localStorage.getItem('hydra_logs');
        if (stored) {
            return JSON.parse(stored);
        }
    } catch (e) {
        console.warn('Failed to load logs from localStorage:', e);
    }
    return [];
};

// Save logs to localStorage
const saveLogs = (logs: LogEntry[]) => {
    try {
        localStorage.setItem('hydra_logs', JSON.stringify(logs.slice(0, 200)));
    } catch (e) {
        console.warn('Failed to save logs to localStorage:', e);
    }
};

export const useHydra = () => {
    const [status, setStatus] = useState<ConnectionStatus>('disconnected');
    const [headState, setHeadState] = useState<HeadState>('Idle');
    const [headId, setHeadId] = useState<string | null>(null);
    const [headUtxos, setHeadUtxos] = useState<Record<string, any>>({});
    const [snapshots, setSnapshots] = useState<Snapshot[]>([]);
    const [peers, setPeers] = useState<Peer[]>([]);
    const [logs, setLogs] = useState<LogEntry[]>(loadLogs);
    const [connectionError, setConnectionError] = useState<string | null>(null);

    const bridgeRef = useRef<HydraBridge | null>(null);

    // Use ref to avoid stale closures in event handlers
    const logsRef = useRef<LogEntry[]>(logs);
    useEffect(() => {
        logsRef.current = logs;
    }, [logs]);

    const addLog = useCallback((message: string, type: LogEntry['type'] = 'info', tag?: string, details?: string) => {
        const timestamp = new Date().toLocaleTimeString('en-US', { hour12: false, hour: '2-digit', minute: '2-digit', second: '2-digit' });
        const fullTimestamp = new Date().toISOString();
        console.log(`[Hydra ${timestamp}] [${tag || type}] ${message}`, details || '');

        const newEntry: LogEntry = {
            timestamp: fullTimestamp,
            type,
            tag,
            message,
            details
        };

        setLogs(prev => {
            const newLogs = [newEntry, ...prev.slice(0, 199)];
            saveLogs(newLogs);
            return newLogs;
        });
    }, []);

    const clearLogs = useCallback(() => {
        setLogs([]);
        saveLogs([]);
    }, []);

    // Process incoming Hydra message
    const processMessage = useCallback((payload: any) => {
        const tag = payload.tag || payload.type || 'Unknown';

        // Log full payload for debugging
        console.log('[Hydra Message]', JSON.stringify(payload, null, 2));

        switch (tag) {
            case 'Greetings':
                // Initial greeting with head status
                const state = payload.headStatus as HeadState;
                if (payload.headStatus) {
                    setHeadState(state);
                }
                if (payload.snapshotUtxo) {
                    setHeadUtxos(payload.snapshotUtxo);
                }
                const greetingsHeadId = payload.headId || payload.hydraHeadId;
                if (greetingsHeadId) {
                    setHeadId(greetingsHeadId);
                }

                // Extract party/peer info from Greetings
                // Structure: me.vkey, env.party.vkey, env.otherParties[].vkey, env.participants[]
                const myVk = payload.me?.vkey;
                const otherParties = payload.env?.otherParties || [];
                const participants = payload.env?.participants || [];
                const networkConnected = payload.networkInfo?.networkConnected || false;

                if (myVk) {
                    // First participant is "me", rest are other parties in order
                    const allPeers: Peer[] = [
                        {
                            vkey: myVk,
                            participant: participants[0],
                            connected: true,
                            isYou: true
                        },
                        ...otherParties.map((p: { vkey: string }, idx: number) => ({
                            vkey: p.vkey,
                            participant: participants[idx + 1],
                            connected: networkConnected,
                            isYou: false
                        }))
                    ];
                    setPeers(allPeers);
                }

                const partyCount = 1 + otherParties.length;
                addLog(
                    `Connected to Hydra node`,
                    'success',
                    'Greetings',
                    `State: ${state}, ${partyCount} parties, network: ${networkConnected ? 'connected' : 'waiting'}`
                );
                break;

            case 'HeadIsInitializing':
                setHeadState('Initializing');
                const initHeadId = payload.headId || payload.hydraHeadId;
                if (initHeadId) {
                    setHeadId(initHeadId);
                }
                if (payload.parties) {
                    setPeers(payload.parties.map((_: any, i: number) => ({
                        name: `Party ${i + 1}`,
                        connected: true,
                        isYou: i === 0
                    })));
                }
                addLog(
                    'Head is initializing',
                    'event',
                    'HeadIsInitializing',
                    initHeadId ? `ID: ${initHeadId.slice(0, 16)}...` : undefined
                );
                break;

            case 'Committed':
                const utxoCount = Object.keys(payload.utxo || {}).length;
                const totalLovelace = Object.values(payload.utxo || {}).reduce((sum: number, u: any) => {
                    return sum + (u.value?.lovelace || 0);
                }, 0);
                addLog(
                    'Party committed',
                    'success',
                    'Committed',
                    `${utxoCount} UTxO(s), ${(totalLovelace / 1_000_000).toFixed(2)} ₳`
                );
                break;

            case 'HeadIsOpen':
                setHeadState('Open');
                setHeadId(payload.headId);
                if (payload.utxo) {
                    setHeadUtxos(payload.utxo);
                }
                const openUtxoCount = Object.keys(payload.utxo || {}).length;
                addLog(
                    'Head is now OPEN!',
                    'success',
                    'HeadIsOpen',
                    `${openUtxoCount} UTxO(s) available`
                );
                break;

            case 'HeadIsClosed':
                setHeadState('Closed');
                addLog('Head is closed', 'warning', 'HeadIsClosed');
                break;

            case 'HeadIsContested':
                addLog('Head is contested', 'warning', 'HeadIsContested');
                break;

            case 'ReadyToFanout':
                setHeadState('FanoutPossible');
                addLog('Ready to fanout', 'info', 'ReadyToFanout');
                break;

            case 'HeadIsFinalized':
                setHeadState('Final');
                setHeadUtxos({});
                addLog('Head is finalized', 'success', 'HeadIsFinalized');
                break;

            case 'HeadIsAborted':
                setHeadState('Idle');
                setHeadId(null);
                setHeadUtxos({});
                addLog('Head aborted', 'warning', 'HeadIsAborted');
                break;

            case 'TxValid':
                const txId = payload.transaction?.txId || payload.txId || 'unknown';
                addLog('Transaction valid', 'tx', 'TxValid', `TX: ${txId.slice(0, 16)}...`);
                break;

            case 'TxInvalid':
                addLog(
                    'Transaction invalid',
                    'error',
                    'TxInvalid',
                    payload.validationError?.reason || 'Unknown reason'
                );
                break;

            case 'SnapshotConfirmed':
                const snapshot = payload.snapshot;
                if (snapshot) {
                    setSnapshots(prev => [{
                        number: snapshot.number || snapshot.snapshotNumber || prev.length + 1,
                        utxo: snapshot.utxo || {},
                        confirmedAt: new Date().toISOString()
                    }, ...prev.slice(0, 50)]);
                    if (snapshot.utxo) {
                        setHeadUtxos(snapshot.utxo);
                    }
                }
                addLog(
                    `Snapshot #${snapshot?.number || snapshot?.snapshotNumber} confirmed`,
                    'success',
                    'SnapshotConfirmed'
                );
                break;

            case 'GetUTxOResponse':
                if (payload.utxo) {
                    setHeadUtxos(payload.utxo);
                }
                addLog(
                    'UTxO response received',
                    'info',
                    'GetUTxOResponse',
                    `${Object.keys(payload.utxo || {}).length} UTxO(s)`
                );
                break;

            case 'CommitRecorded':
                addLog('Commit recorded', 'info', 'CommitRecorded', payload.pendingDeposit);
                break;

            case 'CommitApproved':
                addLog('Commit approved', 'success', 'CommitApproved');
                break;

            case 'CommitFinalized':
                addLog('Commit finalized', 'success', 'CommitFinalized');
                break;

            case 'PeerConnected':
                // Update peer status - peer can be a string or object with vkey
                const connectedVk = typeof payload.peer === 'string'
                    ? payload.peer
                    : payload.peer?.vkey || payload.remoteHost || payload.nodeId || '';
                console.log('[Hydra] PeerConnected:', connectedVk);
                // Mark all non-self peers as connected when any peer connects
                setPeers(prev => prev.map(p =>
                    !p.isYou ? { ...p, connected: true } : p
                ));
                addLog('Peer connected', 'success', 'PeerConnected', connectedVk.slice(0, 20) || 'peer');
                break;

            case 'PeerDisconnected':
                // Update peer status
                const disconnectedVk = typeof payload.peer === 'string'
                    ? payload.peer
                    : payload.peer?.vkey || payload.remoteHost || payload.nodeId || '';
                console.log('[Hydra] PeerDisconnected:', disconnectedVk);
                setPeers(prev => prev.map(p =>
                    !p.isYou ? { ...p, connected: false } : p
                ));
                addLog('Peer disconnected', 'warning', 'PeerDisconnected', disconnectedVk.slice(0, 20) || 'peer');
                break;

            case 'PeerHandshakeFailure':
                addLog('Peer handshake failed', 'error', 'PeerHandshakeFailure', payload.remoteHost || JSON.stringify(payload));
                break;

            case 'NetworkConnected':
                // All peers connected
                console.log('[Hydra] NetworkConnected');
                setPeers(prev => prev.map(p => ({ ...p, connected: true })));
                addLog('Network connected', 'success', 'NetworkConnected');
                break;

            case 'NetworkDisconnected':
                // Mark non-self peers as disconnected
                console.log('[Hydra] NetworkDisconnected');
                setPeers(prev => prev.map(p => p.isYou ? p : { ...p, connected: false }));
                addLog('Network disconnected', 'warning', 'NetworkDisconnected');
                break;

            case 'PeerConnectivityChanged':
                // Handle connectivity changes
                console.log('[Hydra] PeerConnectivityChanged:', payload);
                const isConnected = payload.connected || payload.networkConnected;
                setPeers(prev => prev.map(p =>
                    !p.isYou ? { ...p, connected: isConnected } : p
                ));
                addLog(
                    isConnected ? 'Peer connectivity restored' : 'Peer connectivity lost',
                    isConnected ? 'success' : 'warning',
                    'PeerConnectivity'
                );
                break;

            default:
                // Log ALL events so we don't miss anything
                addLog(
                    `Event: ${tag}`,
                    'event',
                    tag,
                    JSON.stringify(payload).slice(0, 200)
                );
                break;
        }
    }, [addLog]);

    const connect = useCallback(async (url: string) => {
        // Clear any previous connection error
        setConnectionError(null);

        // Disconnect existing bridge if any
        if (bridgeRef.current) {
            try {
                await bridgeRef.current.disconnect();
            } catch (e) {
                console.warn('Error disconnecting old bridge:', e);
            }
        }

        // Add ?history=yes to get all historical events on connect
        const wsUrl = url.includes('?') ? `${url}&history=yes` : `${url}?history=yes`;

        setStatus('connecting');
        addLog('Connecting...', 'info', 'Connection', wsUrl);

        try {
            // Create new HydraBridge instance using the SDK
            const bridge = new HydraBridge({
                url: wsUrl,
                verbose: true
            });

            bridgeRef.current = bridge;

            // Set up event handlers BEFORE connecting
            bridge.events.on('onConnected', () => {
                setStatus('connected');
                setConnectionError(null);
                addLog('WebSocket connected', 'success', 'Connection');
            });

            bridge.events.on('onDisconnected', () => {
                setStatus('disconnected');
                addLog('Disconnected', 'warning', 'Connection');
            });

            bridge.events.on('onConnectError', (error: any) => {
                setStatus('disconnected');
                const errorMsg = error?.message || 'Connection failed. Is the Hydra node running?';
                setConnectionError(errorMsg);
                addLog('Connection error', 'error', 'Connection', errorMsg);
            });

            bridge.events.on('onMessage', (payload: any) => {
                console.log('[Hydra] Raw message received:', payload);
                processMessage(payload);
            });

            // Connect and wait with timeout
            const connected = await bridge.connect();

            if (connected) {
                setStatus('connected');
                setConnectionError(null);
            } else {
                // Connection returned false - node not reachable
                setConnectionError('Failed to connect. Make sure the Hydra node is running.');
            }

        } catch (e) {
            const error = e instanceof Error ? e.message : String(e);
            setConnectionError(`Connection failed: ${error}`);
            addLog('Connection failed', 'error', 'Connection', error);
            setStatus('disconnected');
        }
    }, [addLog, processMessage]);

    const disconnect = useCallback(async () => {
        if (bridgeRef.current) {
            try {
                await bridgeRef.current.disconnect();
            } catch (e) {
                console.warn('Error disconnecting:', e);
            }
            bridgeRef.current = null;
        }
        setStatus('disconnected');
        setHeadState('Idle');
        setHeadId(null);
        setHeadUtxos({});
        setPeers([]);
    }, []);

    // SDK Commands
    const init = useCallback(() => {
        console.log('[Hydra] Init called');

        if (bridgeRef.current?.connected()) {
            addLog('Sending Init command', 'info', 'Command');
            try {
                const result = bridgeRef.current.commands.init();
                console.log('[Hydra] Init command result:', result);
                addLog('Init command sent', 'success', 'Command');
            } catch (e) {
                console.error('[Hydra] Init error:', e);
                addLog('Init failed', 'error', 'Command', String(e));
            }
        } else {
            addLog('Cannot init: not connected', 'error', 'Command');
        }
    }, [addLog]);

    const abort = useCallback(() => {
        if (bridgeRef.current?.connected()) {
            addLog('Sending Abort command', 'warning', 'Command');
            bridgeRef.current.commands.abort();
        }
    }, [addLog]);

    const closeHead = useCallback(() => {
        if (bridgeRef.current?.connected()) {
            addLog('Sending Close command', 'warning', 'Command');
            bridgeRef.current.commands.close();
        }
    }, [addLog]);

    const fanout = useCallback(() => {
        if (bridgeRef.current?.connected()) {
            addLog('Sending Fanout command', 'info', 'Command');
            bridgeRef.current.commands.fanout();
        }
    }, [addLog]);

    // Submit new transaction to head using SDK
    const sendRawTx = useCallback((cborHex: string, description?: string) => {
        if (bridgeRef.current?.connected()) {
            addLog('Submitting transaction', 'tx', 'NewTx', cborHex.substring(0, 24) + '...');
            bridgeRef.current.commands.newTx(cborHex, description || 'Transaction');
        } else {
            addLog('Cannot submit: not connected', 'error', 'NewTx');
        }
    }, [addLog]);

    // Query UTxOs for an address in the head
    const queryAddressUtxo = useCallback(async (address: string): Promise<UTxO[]> => {
        if (!bridgeRef.current?.connected()) {
            throw new Error('Not connected to Hydra node');
        }
        return await bridgeRef.current.queryAddressUTxO(address);
    }, []);

    // Cleanup on unmount
    useEffect(() => {
        return () => {
            if (bridgeRef.current) {
                bridgeRef.current.disconnect();
            }
        };
    }, []);

    // Clear connection error
    const clearConnectionError = useCallback(() => {
        setConnectionError(null);
    }, []);

    return {
        // State
        status,
        headState,
        headId,
        headUtxos,
        snapshots,
        peers,
        logs,
        connectionError,

        // Connection
        connect,
        disconnect,
        clearConnectionError,

        // Commands
        init,
        abort,
        closeHead,
        fanout,
        sendRawTx,

        // Queries
        queryAddressUtxo,

        // Utils
        clearLogs,

        // Bridge reference (for advanced usage)
        bridge: bridgeRef.current
    };
};
