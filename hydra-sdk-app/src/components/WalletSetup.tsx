import React from 'react';
import { Button, TextArea, FileInput } from './ui';

interface WalletSetupProps {
  mnemonicInput: string;
  onMnemonicInputChange: (value: string) => void;
  onGenerateWallet: () => void;
  onRestoreWallet: () => void;
  onFileChange: (event: React.ChangeEvent<HTMLInputElement>) => void;
}

export const WalletSetup: React.FC<WalletSetupProps> = ({
  mnemonicInput,
  onMnemonicInputChange,
  onGenerateWallet,
  onRestoreWallet,
  onFileChange,
}) => {
  return (
    <div className="mt-4 space-y-4">
      <h3 className="text-slate-200 font-medium">🔐 Wallet Setup</h3>

      {/* Option 1: Generate */}
      <div className="space-y-2">
        <div className="text-sm font-medium text-slate-200">Option 1: Generate New Wallet</div>
        <p className="text-xs text-slate-500">Create a new random signing key</p>
        <Button fullWidth onClick={onGenerateWallet}>
          ✨ Generate New Wallet
        </Button>
      </div>

      <div className="relative text-center text-slate-500 text-sm">
        <span className="relative z-10 bg-dark-card px-3">or</span>
        <div className="absolute left-0 right-0 top-1/2 h-px bg-border -z-0" />
      </div>

      {/* Option 2: Restore */}
      <div className="space-y-2">
        <div className="text-sm font-medium text-slate-200">Option 2: Restore from Private Key</div>
        <TextArea
          placeholder="Enter your 64-character hex private key..."
          value={mnemonicInput}
          onChange={(e) => onMnemonicInputChange(e.target.value)}
          rows={2}
        />
        <Button
          variant="secondary"
          fullWidth
          onClick={onRestoreWallet}
          disabled={!mnemonicInput.trim()}
        >
          🔄 Restore Wallet
        </Button>
      </div>

      <div className="relative text-center text-slate-500 text-sm">
        <span className="relative z-10 bg-dark-card px-3">or</span>
        <div className="absolute left-0 right-0 top-1/2 h-px bg-border -z-0" />
      </div>

      {/* Option 3: Load file */}
      <div className="space-y-2">
        <div className="text-sm font-medium text-slate-200">Option 3: Load .sk File</div>
        <p className="text-xs text-slate-500">Use an existing Cardano signing key</p>
        <FileInput accept=".sk" onChange={onFileChange} />
      </div>
    </div>
  );
};

