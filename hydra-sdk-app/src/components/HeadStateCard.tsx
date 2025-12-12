import React from 'react';
import { Card, ActionButton, HeadStateBadge, Button } from './ui';
import type { Peer } from '../hooks/useHydra';

type HeadState = 'Idle' | 'Initializing' | 'Open' | 'Closed' | 'FanoutPossible' | 'Final';

interface HeadStateCardProps {
  headState: HeadState;
  headId: string | null;
  headBalance: number;
  peers: Peer[];
  txRecipient: string;
  txAmount: string;
  txSubmitting: boolean;
  onInit: () => void;
  onCommit: () => void;
  onAbort: () => void;
  onClose: () => void;
  onFanout: () => void;
  onTxRecipientChange: (value: string) => void;
  onTxAmountChange: (value: string) => void;
  onSubmitTx: () => void;
}

const formatAda = (lovelace: number) => {
  return (lovelace / 1_000_000).toLocaleString(undefined, {
    minimumFractionDigits: 2,
    maximumFractionDigits: 6
  });
};

export const HeadStateCard: React.FC<HeadStateCardProps> = ({
  headState,
  headId,
  headBalance,
  peers,
  txRecipient,
  txAmount,
  txSubmitting,
  onInit,
  onCommit,
  onAbort,
  onClose,
  onFanout,
  onTxRecipientChange,
  onTxAmountChange,
  onSubmitTx,
}) => {
  return (
    <Card className="p-4">
      {/* Top Row: State + Actions */}
      <div className="flex flex-col sm:flex-row sm:items-start sm:justify-between gap-4">
        <div className="flex flex-col gap-1.5 min-w-0">
          <HeadStateBadge state={headState} />
          {headId && (
            <span className="text-xs text-slate-500">
              ID: <code className="text-accent text-[10px]">{headId.slice(0, 12)}...{headId.slice(-8)}</code>
            </span>
          )}
        </div>

        {/* Action Buttons */}
        <div className="grid grid-cols-3 gap-1.5 w-full sm:w-60">
          <ActionButton action="init" onClick={onInit} disabled={headState !== 'Idle'}>
            🚀 Init
          </ActionButton>
          <ActionButton action="commit" onClick={onCommit} disabled={headState !== 'Initializing'}>
            📥 Commit
          </ActionButton>
          <ActionButton action="abort" onClick={onAbort} disabled={headState !== 'Initializing'}>
            ✕ Abort
          </ActionButton>
          <ActionButton action="close" onClick={onClose} disabled={headState !== 'Open'}>
            🔒 Close
          </ActionButton>
          <ActionButton action="fanout" onClick={onFanout} disabled={headState !== 'FanoutPossible' && headState !== 'Closed'}>
            📤 Fanout
          </ActionButton>
        </div>
      </div>

      {/* Peers Section */}
      {peers.length > 0 && (
        <div className="mt-4 pt-4 border-t border-border">
          <div className="text-xs font-semibold text-slate-500 mb-2">Parties ({peers.length})</div>
          <div className="space-y-1.5">
            {peers.map((peer, idx) => (
              <div
                key={idx}
                className={`
                  flex items-center gap-2.5 px-3 py-2 rounded-md
                  bg-dark-bg border
                  ${peer.isYou ? 'border-accent bg-accent/5' : 'border-border'}
                `}
              >
                <span className={`text-xs ${peer.connected ? 'text-success' : 'text-slate-500'}`}>
                  {peer.connected ? '●' : '○'}
                </span>
                <span className="font-semibold text-sm text-slate-200 min-w-[70px]">
                  {peer.isYou ? 'You' : `Peer ${idx}`}
                </span>
                <code className="flex-1 text-[10px] text-accent bg-dark-card px-2.5 py-1 rounded break-all">
                  {peer.vkey}
                </code>
                <span className={`
                  text-[10px] px-2.5 py-1 rounded-xl font-medium shrink-0
                  ${peer.connected ? 'bg-success/15 text-success' : 'bg-slate-500/15 text-slate-500'}
                `}>
                  {peer.connected ? 'Connected' : 'Waiting'}
                </span>
              </div>
            ))}
          </div>
        </div>
      )}

      {/* Transaction Form (when head is open) */}
      {headState === 'Open' && (
        <div className="mt-4 pt-4 border-t border-border">
          <div className="flex flex-wrap items-center gap-3 p-3 bg-dark-card border border-border rounded-lg">
            <div className="flex items-center gap-2 pr-3 border-r border-border">
              <span className="text-xs text-slate-500">In Head:</span>
              <span className="text-sm font-semibold text-success">{formatAda(headBalance)} ₳</span>
            </div>
            <input
              type="text"
              value={txRecipient}
              onChange={(e) => onTxRecipientChange(e.target.value)}
              placeholder="Recipient addr_test1..."
              className="flex-[2] min-w-[120px] bg-dark-input border border-border rounded-md px-2.5 py-2 text-sm text-slate-200 focus:outline-none focus:border-accent"
            />
            <input
              type="number"
              value={txAmount}
              onChange={(e) => onTxAmountChange(e.target.value)}
              placeholder="Amount (ADA)"
              className="flex-1 min-w-[100px] max-w-[140px] bg-dark-input border border-border rounded-md px-2.5 py-2 text-sm text-slate-200 focus:outline-none focus:border-accent"
            />
            <Button
              size="sm"
              onClick={onSubmitTx}
              disabled={txSubmitting || !txRecipient || !txAmount}
            >
              {txSubmitting ? '...' : 'Send'}
            </Button>
          </div>
        </div>
      )}
    </Card>
  );
};

