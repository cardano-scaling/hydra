import React from 'react';

export const Header: React.FC = () => {
  return (
    <header className="text-center mb-8 py-2 border-b border-border">
      <h1 className="text-3xl md:text-4xl font-light text-accent mb-1 tracking-tight">
        🐉 Hydra Lightweight Demo
      </h1>
      <p className="text-slate-400 text-sm md:text-base">
        Interact with Hydra Head using Blockfrost • No cardano-node required
      </p>
    </header>
  );
};

