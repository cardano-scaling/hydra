import React, { useState } from 'react';
import { Card, Button } from './ui';
import { LogEntry } from './LogEntry';
import type { LogEntry as LogEntryType, Snapshot } from '../hooks/useHydra';

interface TabsPanelProps {
  headUtxos: Record<string, any>;
  snapshots: Snapshot[];
  logs: LogEntryType[];
  address: string | null;
  onClearLogs: () => void;
}

type TabType = 'utxos' | 'snapshots' | 'logs';

const formatAda = (lovelace: string | number) => {
  return (Number(lovelace) / 1_000_000).toLocaleString(undefined, {
    minimumFractionDigits: 2,
    maximumFractionDigits: 6
  });
};

export const TabsPanel: React.FC<TabsPanelProps> = ({
  headUtxos,
  snapshots,
  logs,
  address,
  onClearLogs,
}) => {
  const [activeTab, setActiveTab] = useState<TabType>('utxos');

  const tabs: { key: TabType; label: string; count: number }[] = [
    { key: 'utxos', label: 'UTxOs', count: Object.keys(headUtxos).length },
    { key: 'snapshots', label: 'Snapshots', count: snapshots.length },
    { key: 'logs', label: 'Event Log', count: logs.length },
  ];

  return (
    <Card className="flex-1 min-h-[300px] flex flex-col">
      {/* Tabs */}
      <div className="flex items-center border-b border-border mb-3 gap-1">
        {tabs.map(tab => (
          <button
            key={tab.key}
            className={`
              px-4 py-2.5 text-sm border-b-2 -mb-px transition-all
              ${activeTab === tab.key
                ? 'text-accent border-accent'
                : 'text-slate-400 border-transparent hover:text-slate-200 hover:bg-dark-hover'
              }
            `}
            onClick={() => setActiveTab(tab.key)}
          >
            {tab.label} ({tab.count})
          </button>
        ))}
        {activeTab === 'logs' && logs.length > 0 && (
          <Button
            variant="danger"
            size="sm"
            className="ml-auto text-[10px] px-2 py-1"
            onClick={onClearLogs}
          >
            Clear
          </Button>
        )}
      </div>

      {/* Tab Content */}
      <div className="flex-1 overflow-y-auto max-h-[calc(100vh-500px)] min-h-[200px]">
        {activeTab === 'utxos' && (
          <div className="space-y-2">
            {Object.keys(headUtxos).length === 0 ? (
              <div className="text-center py-10 text-slate-500">
                No UTxOs in head yet. Commit some funds first!
              </div>
            ) : (
              Object.entries(headUtxos).map(([txIn, output]: [string, any]) => {
                const lovelace = output.value?.lovelace || output.value || 0;
                const isOwn = output.address === address;
                return (
                  <div
                    key={txIn}
                    className={`
                      grid grid-cols-1 md:grid-cols-[1fr_1fr_auto] gap-3
                      p-3 rounded-md bg-dark-bg border
                      ${isOwn ? 'border-accent' : 'border-border'}
                    `}
                  >
                    <div>
                      <div className="text-sm text-info font-mono">{txIn}</div>
                      <div className="text-xs text-slate-500 font-mono">
                        {output.address?.slice(0, 30)}...
                        {isOwn && <span className="text-accent ml-1">(you)</span>}
                      </div>
                    </div>
                    <div className="text-sm text-success font-medium text-right">
                      {formatAda(lovelace)} ₳
                    </div>
                  </div>
                );
              })
            )}
          </div>
        )}

        {activeTab === 'snapshots' && (
          <div className="space-y-2">
            {snapshots.length === 0 ? (
              <div className="text-center py-10 text-slate-500">
                No snapshots confirmed yet.
              </div>
            ) : (
              <>
                <div className="p-3 bg-accent/10 border border-accent rounded-md text-center">
                  <strong>Latest Snapshot:</strong> #{snapshots[0]?.number}
                </div>
                {snapshots.map((snap, idx) => (
                  <div key={idx} className="p-3 bg-dark-bg border border-border rounded-md">
                    <div className="flex justify-between mb-1">
                      <span className="font-medium text-accent">Snapshot #{snap.number}</span>
                      <span className="text-xs text-slate-500">{snap.confirmedAt}</span>
                    </div>
                    <div className="text-sm text-slate-400">
                      {Object.keys(snap.utxo || {}).length} UTxO(s)
                    </div>
                  </div>
                ))}
              </>
            )}
          </div>
        )}

        {activeTab === 'logs' && (
          <div className="space-y-1.5">
            {logs.length === 0 ? (
              <div className="text-center py-10 text-slate-500">
                No events yet. Connect to a Hydra node to see activity.
              </div>
            ) : (
              logs.map((entry, idx) => (
                <LogEntry key={idx} entry={entry} />
              ))
            )}
          </div>
        )}
      </div>
    </Card>
  );
};

