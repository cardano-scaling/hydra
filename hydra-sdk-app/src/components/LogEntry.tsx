import React from 'react';
import type { LogEntry as LogEntryType } from '../hooks/useHydra';

const formatLogTime = (isoString: string): string => {
  try {
    const date = new Date(isoString);
    return date.toLocaleTimeString('en-US', {
      hour12: false,
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit'
    });
  } catch {
    return isoString;
  }
};

const typeColors: Record<LogEntryType['type'], string> = {
  info: 'text-info border-l-info',
  success: 'text-success border-l-success',
  warning: 'text-warning border-l-warning',
  error: 'text-danger border-l-danger bg-danger/5',
  event: 'text-accent border-l-accent',
  tx: 'text-purple-400 border-l-purple-400'
};

const typeIcons: Record<LogEntryType['type'], string> = {
  info: 'ℹ️',
  success: '✅',
  warning: '⚠️',
  error: '❌',
  event: '📡',
  tx: '💳'
};

interface LogEntryProps {
  entry: LogEntryType;
}

export const LogEntry: React.FC<LogEntryProps> = ({ entry }) => {
  const colorClass = typeColors[entry.type];
  const icon = typeIcons[entry.type];

  return (
    <div className={`
      p-2.5 rounded-r-md border-l-[3px] bg-dark-bg
      hover:bg-dark-hover transition-colors
      ${colorClass}
    `}>
      <div className="flex items-center gap-2 mb-1">
        <span className="text-sm">{icon}</span>
        {entry.tag && (
          <span className={`text-[10px] font-semibold uppercase tracking-wider px-1.5 py-0.5 bg-white/5 rounded ${colorClass.split(' ')[0]}`}>
            {entry.tag}
          </span>
        )}
        <span className="text-[10px] text-slate-500 ml-auto font-mono">
          {formatLogTime(entry.timestamp)}
        </span>
      </div>
      <div className="text-sm text-slate-200 font-medium">{entry.message}</div>
      {entry.details && (
        <div className="text-xs text-slate-500 mt-1 font-mono break-all">
          {entry.details}
        </div>
      )}
    </div>
  );
};

