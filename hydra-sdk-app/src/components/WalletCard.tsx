import React, { useState } from 'react';
import { Button } from './ui';

interface UTxO {
  txHash: string;
  outputIndex: number;
  address: string;
  amount: { unit: string; quantity: string }[];
}

interface WalletCardProps {
  address: string;
  walletMode: string;
  l1Utxos: UTxO[];
  l1Loading: boolean;
  mnemonic: string | null;
  showMnemonic: boolean;
  splittingUtxo: string | null; // txHash#outputIndex of currently splitting UTxO
  onRefresh: () => void;
  onExportSk: () => void;
  onExportVk: () => void;
  onDisconnect: () => void;
  onSplitUtxo: (utxo: UTxO) => void;
  onHideMnemonic: () => void;
  onShowNodeSetup: () => void;
}

const formatAda = (lovelace: string | number) => {
  return (Number(lovelace) / 1_000_000).toLocaleString(undefined, {
    minimumFractionDigits: 2,
    maximumFractionDigits: 6
  });
};

export const WalletCard: React.FC<WalletCardProps> = ({
  address,
  walletMode,
  l1Utxos,
  l1Loading,
  mnemonic,
  showMnemonic,
  splittingUtxo,
  onRefresh,
  onExportSk,
  onExportVk,
  onDisconnect,
  onSplitUtxo,
  onHideMnemonic,
  onShowNodeSetup,
}) => {
  const [showUtxoDropdown, setShowUtxoDropdown] = useState(false);

  const totalBalance = l1Utxos.reduce((sum, u) =>
    sum + parseInt(u.amount.find(a => a.unit === 'lovelace')?.quantity || '0'), 0
  );

  return (
    <div className="mt-4 bg-dark-bg border border-accent rounded-lg p-3 space-y-3">
      {/* Header */}
      <div className="flex justify-between items-center">
        <span className="text-success font-medium">✅ Wallet Connected</span>
        <span className="text-xs text-slate-500">({walletMode})</span>
      </div>

      {/* Address */}
      <div>
        <div className="text-xs text-slate-500 mb-1">Address</div>
        <div className="flex items-start gap-2">
          <div className="text-sm text-accent break-all flex-1">{address}</div>
          <button
            className="shrink-0 bg-dark-bg border border-border rounded px-2 py-1 text-xs hover:bg-accent hover:border-accent transition-colors"
            onClick={() => navigator.clipboard.writeText(address)}
            title="Copy address"
          >
            📋
          </button>
        </div>
      </div>

      {/* L1 Balance */}
      <div>
        <div className="text-xs text-slate-500 mb-1">L1 Balance</div>
        <div className="text-success font-medium flex items-center gap-2">
          {l1Loading ? 'Loading...' : `${formatAda(totalBalance)} ₳`}
          <button
            className="text-accent hover:text-accent-hover transition-colors"
            onClick={onRefresh}
            disabled={l1Loading}
          >
            ⟳
          </button>
        </div>
      </div>

      {/* UTxO Section */}
      <div className="bg-dark-bg border border-border rounded-md">
        <div
          className="flex items-center justify-between px-3 py-2.5 cursor-pointer hover:bg-dark-hover transition-colors"
          onClick={() => setShowUtxoDropdown(!showUtxoDropdown)}
        >
          <span className="text-sm text-slate-400 flex items-center gap-2">
            <span className="text-xs text-slate-500">{showUtxoDropdown ? '▼' : '▶'}</span>
            {l1Utxos.length} UTxO{l1Utxos.length !== 1 ? 's' : ''}
          </span>
        </div>

        {showUtxoDropdown && l1Utxos.length > 0 && (
          <div className="border-t border-border max-h-64 overflow-y-auto">
            {l1Utxos.map((utxo, idx) => {
              const lovelace = parseInt(utxo.amount.find(a => a.unit === 'lovelace')?.quantity || '0');
              const tokens = utxo.amount.filter(a => a.unit !== 'lovelace');
              const utxoId = `${utxo.txHash}#${utxo.outputIndex}`;
              const isSplitting = splittingUtxo === utxoId;
              // Need at least 4.5 ADA to split into 2 (2 ADA min per output + fees)
              const canSplit = lovelace >= 4_500_000;
              
              return (
                <div key={utxoId} className="px-3 py-2.5 border-b border-border last:border-b-0">
                  <div className="flex justify-between items-center mb-1">
                    <span className="text-xs text-slate-500 bg-dark-card px-1.5 py-0.5 rounded">#{idx + 1}</span>
                    <div className="flex items-center gap-2">
                      <span className="text-sm text-success font-semibold">{formatAda(lovelace)} ₳</span>
                      {canSplit && (
                        <button
                          className="bg-gradient-to-br from-warning to-orange-600 px-2 py-1 rounded text-[10px] font-medium text-black hover:scale-105 transition-transform disabled:opacity-60 disabled:hover:scale-100"
                          onClick={(e) => { e.stopPropagation(); onSplitUtxo(utxo); }}
                          disabled={splittingUtxo !== null}
                          title="Split this UTxO into 2"
                        >
                          {isSplitting ? '⏳...' : '✂️ Split'}
                        </button>
                      )}
                    </div>
                  </div>
                  <div className="text-xs text-slate-500 font-mono" title={utxoId}>
                    {utxo.txHash.slice(0, 16)}...#{utxo.outputIndex}
                  </div>
                  {tokens.length > 0 && (
                    <div className="mt-1 flex flex-wrap gap-1">
                      {tokens.map(t => (
                        <span key={t.unit} className="text-xs bg-info/20 text-info px-1.5 py-0.5 rounded">
                          +{t.quantity} token
                        </span>
                      ))}
                    </div>
                  )}
                </div>
              );
            })}
          </div>
        )}
      </div>

      {/* Mnemonic Display */}
      {showMnemonic && mnemonic && (
        <div className="bg-warning/10 border border-warning rounded-md p-3">
          <div className="text-sm text-warning font-medium mb-2">⚠️ Save this key! It won't be shown again.</div>
          <div className="font-mono text-xs text-slate-200 break-all mb-3">{mnemonic}</div>
          <Button
            variant="secondary"
            size="sm"
            onClick={() => {
              const keyMatch = mnemonic.match(/Private Key \(hex\): ([0-9a-fA-F]+)/);
              navigator.clipboard.writeText(keyMatch ? keyMatch[1] : mnemonic);
              onHideMnemonic();
            }}
          >
            📋 Copy & Hide
          </Button>
        </div>
      )}

      {/* Actions */}
      <div className="flex flex-wrap gap-2 pt-3 border-t border-border">
        <Button variant="secondary" size="sm" onClick={onExportSk}>💾 Export .sk</Button>
        <Button variant="secondary" size="sm" onClick={onExportVk}>📤 Export .vk</Button>
        <Button variant="secondary" size="sm" onClick={onDisconnect}>🔓 Disconnect</Button>
      </div>

      <Button variant="secondary" fullWidth onClick={onShowNodeSetup}>
        🚀 How to Start Hydra Node
      </Button>
    </div>
  );
};

