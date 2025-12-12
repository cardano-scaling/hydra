import React from 'react';
import { Modal } from './ui';

interface NodeSetupModalProps {
  isOpen: boolean;
  onClose: () => void;
}

const getHydraNodeCommand = () => {
  const scriptsTxId = "ee449c99464c5419954f39b98e513b17406e24c9883e6342e073006e54878524,d6e03afa86cf1d74011ba234ec32fbd102d4332c3891a49419dae318281bc96a,0b32f7cf144090b3a2d6787cb5b4cabbc0a72c1ae77bf72de8e3d9aa9476bfb7";
  return `hydra-node \\
  --node-id 1 \\
  --api-host 0.0.0.0 \\
  --api-port 4001 \\
  --blockfrost ./blockfrost-project.txt \\
  --hydra-signing-key ./hydra.sk \\
  --cardano-signing-key ./cardano.sk \\
  --hydra-scripts-tx-id "${scriptsTxId}" \\
  --ledger-protocol-parameters ./protocol-parameters.json \\
  --persistence-dir ./persistence \\
  --contestation-period 60s`;
};

export const NodeSetupModal: React.FC<NodeSetupModalProps> = ({ isOpen, onClose }) => {
  return (
    <Modal isOpen={isOpen} onClose={onClose} title="🚀 Start Hydra Node Locally" maxWidth="xl">
      <p className="text-sm text-slate-400 mb-4">
        Export your keys from the wallet section above, then run this command to start a single-party Hydra head:
      </p>

      {/* Command Block */}
      <div className="relative bg-dark-bg rounded-lg p-4 mb-4 border border-border">
        <pre className="font-mono text-xs text-slate-400 whitespace-pre-wrap break-all leading-relaxed">
          {getHydraNodeCommand()}
        </pre>
        <button
          className="absolute top-2 right-2 px-2 py-1 text-[10px] bg-dark-hover border border-border rounded text-slate-400 hover:bg-accent hover:text-dark-bg hover:border-accent transition-colors"
          onClick={() => navigator.clipboard.writeText(getHydraNodeCommand())}
        >
          📋 Copy
        </button>
      </div>

      {/* Prerequisites */}
      <div className="bg-accent/10 border-l-[3px] border-accent rounded-r-md p-4 mb-4 text-sm text-slate-400">
        <strong className="text-slate-200">Prerequisites:</strong>
        <ul className="mt-2 ml-4 space-y-1 list-disc">
          <li><code className="bg-dark-bg px-1.5 py-0.5 rounded text-xs">hydra.sk</code> — Click <strong>"🔑 Generate Hydra Keys"</strong> above, then export</li>
          <li><code className="bg-dark-bg px-1.5 py-0.5 rounded text-xs">blockfrost-project.txt</code> — Your Blockfrost API key</li>
          <li><code className="bg-dark-bg px-1.5 py-0.5 rounded text-xs">protocol-parameters.json</code> — Fetch from Blockfrost or cardano-cli</li>
          <li><code className="bg-dark-bg px-1.5 py-0.5 rounded text-xs">cardano.sk</code> — Export from wallet above (needs ~2 ADA for fuel)</li>
        </ul>
      </div>

      {/* Tip */}
      <div className="bg-accent/10 border-l-[3px] border-accent rounded-r-md p-4 text-sm text-slate-400">
        <strong>💡 Tip:</strong> The <code className="bg-dark-bg px-1.5 py-0.5 rounded text-xs">cardano.sk</code> exported from your wallet must have ADA to pay L1 transaction fees.
        Fund it via{' '}
        <a href="https://docs.cardano.org/cardano-testnets/tools/faucet" target="_blank" rel="noopener noreferrer" className="text-accent hover:underline">
          Cardano Faucet
        </a>.
      </div>
    </Modal>
  );
};

