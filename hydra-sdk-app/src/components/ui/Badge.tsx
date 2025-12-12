import React from 'react';

type BadgeVariant = 'success' | 'warning' | 'danger' | 'info' | 'default';

interface BadgeProps {
  variant?: BadgeVariant;
  children: React.ReactNode;
  className?: string;
}

const variantStyles: Record<BadgeVariant, string> = {
  success: 'bg-success/20 text-success',
  warning: 'bg-warning/20 text-warning',
  danger: 'bg-danger/20 text-danger',
  info: 'bg-info/20 text-info',
  default: 'bg-slate-500/20 text-slate-400',
};

export const Badge: React.FC<BadgeProps> = ({ variant = 'default', children, className = '' }) => {
  return (
    <span className={`text-xs px-2 py-1 rounded inline-block ${variantStyles[variant]} ${className}`}>
      {children}
    </span>
  );
};

// Status badge for connection states
type StatusType = 'connected' | 'connecting' | 'disconnected';

const statusStyles: Record<StatusType, string> = {
  connected: 'bg-success/20 text-success',
  connecting: 'bg-warning/20 text-warning',
  disconnected: 'bg-slate-500/20 text-slate-400',
};

interface StatusBadgeProps {
  status: StatusType;
  className?: string;
}

export const StatusBadge: React.FC<StatusBadgeProps> = ({ status, className = '' }) => {
  return (
    <span className={`px-2.5 py-1 rounded-xl text-xs font-medium ${statusStyles[status]} ${className}`}>
      {status}
    </span>
  );
};

// Head state badge
type HeadStateType = 'Idle' | 'Initializing' | 'Open' | 'Closed' | 'FanoutPossible' | 'Final';

const headStateStyles: Record<HeadStateType, string> = {
  Idle: 'bg-slate-500/20 text-slate-400',
  Initializing: 'bg-warning/20 text-warning',
  Open: 'bg-success/20 text-success',
  Closed: 'bg-danger/20 text-danger',
  FanoutPossible: 'bg-info/20 text-info',
  Final: 'bg-purple-400/20 text-purple-400',
};

interface HeadStateBadgeProps {
  state: HeadStateType;
  className?: string;
}

export const HeadStateBadge: React.FC<HeadStateBadgeProps> = ({ state, className = '' }) => {
  return (
    <span className={`
      px-3 py-1.5 rounded-2xl text-xs font-semibold uppercase tracking-wide
      ${headStateStyles[state]} ${className}
    `}>
      {state}
    </span>
  );
};

