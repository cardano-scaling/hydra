import React from 'react';

interface CardProps {
  children: React.ReactNode;
  className?: string;
}

export const Card: React.FC<CardProps> = ({ children, className = '' }) => {
  return (
    <div className={`bg-dark-card border border-border rounded-xl p-5 ${className}`}>
      {children}
    </div>
  );
};

interface CardHeaderProps {
  children: React.ReactNode;
  className?: string;
}

export const CardHeader: React.FC<CardHeaderProps> = ({ children, className = '' }) => {
  return (
    <h2 className={`text-lg font-medium text-slate-200 mb-4 flex items-center gap-2 ${className}`}>
      {children}
    </h2>
  );
};

