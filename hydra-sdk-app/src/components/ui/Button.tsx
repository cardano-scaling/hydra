import React from 'react';

type ButtonVariant = 'primary' | 'secondary' | 'danger' | 'ghost';
type ButtonSize = 'sm' | 'md' | 'lg';

interface ButtonProps extends React.ButtonHTMLAttributes<HTMLButtonElement> {
  variant?: ButtonVariant;
  size?: ButtonSize;
  fullWidth?: boolean;
  children: React.ReactNode;
}

const variantStyles: Record<ButtonVariant, string> = {
  primary: 'bg-accent text-dark-bg hover:bg-accent-hover font-medium',
  secondary: 'bg-dark-hover border border-border text-slate-200 hover:bg-border',
  danger: 'bg-danger text-white hover:opacity-90',
  ghost: 'bg-transparent text-accent hover:text-accent-hover',
};

const sizeStyles: Record<ButtonSize, string> = {
  sm: 'px-3 py-1.5 text-xs',
  md: 'px-4 py-2.5 text-sm',
  lg: 'px-6 py-3 text-base',
};

export const Button: React.FC<ButtonProps> = ({
  variant = 'primary',
  size = 'md',
  fullWidth = false,
  disabled,
  className = '',
  children,
  ...props
}) => {
  return (
    <button
      className={`
        rounded-md transition-all duration-200 cursor-pointer
        disabled:opacity-40 disabled:cursor-not-allowed
        ${variantStyles[variant]}
        ${sizeStyles[size]}
        ${fullWidth ? 'w-full' : ''}
        ${className}
      `}
      disabled={disabled}
      {...props}
    >
      {children}
    </button>
  );
};

// Action button variants for head operations
type ActionVariant = 'init' | 'commit' | 'abort' | 'close' | 'fanout';

const actionStyles: Record<ActionVariant, string> = {
  init: 'bg-gradient-to-br from-accent to-accent-hover text-dark-bg',
  commit: 'bg-gradient-to-br from-info to-blue-500 text-white',
  abort: 'bg-dark-hover border border-danger text-danger',
  close: 'bg-gradient-to-br from-warning to-orange-600 text-white',
  fanout: 'bg-gradient-to-br from-purple-400 to-purple-600 text-white',
};

interface ActionButtonProps extends React.ButtonHTMLAttributes<HTMLButtonElement> {
  action: ActionVariant;
  children: React.ReactNode;
}

export const ActionButton: React.FC<ActionButtonProps> = ({
  action,
  disabled,
  className = '',
  children,
  ...props
}) => {
  return (
    <button
      className={`
        px-2 py-1.5 text-xs font-medium rounded transition-all duration-200
        disabled:opacity-40 disabled:cursor-not-allowed
        ${actionStyles[action]}
        ${className}
      `}
      disabled={disabled}
      {...props}
    >
      {children}
    </button>
  );
};

