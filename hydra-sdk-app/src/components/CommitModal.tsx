import React, { useState } from 'react';
import { Modal, Button } from './ui';

interface UTxO {
    txHash: string;
    outputIndex: number;
    address: string;
    amount: { unit: string; quantity: string }[];
}

interface CommitModalProps {
    isOpen: boolean;
    l1Utxos: UTxO[];
    onClose: () => void;
    onCommit: (selectedUtxos: UTxO[]) => void;
    onRefresh: () => void;
    loading: boolean;
}

const formatAda = (lovelace: string | number) => {
    return (Number(lovelace) / 1_000_000).toLocaleString(undefined, {
        minimumFractionDigits: 2,
        maximumFractionDigits: 6
    });
};

export const CommitModal: React.FC<CommitModalProps> = ({
    isOpen,
    l1Utxos,
    onClose,
    onCommit,
    onRefresh,
    loading
}) => {
    const [selectedUtxos, setSelectedUtxos] = useState<Set<string>>(new Set());
    const [isSubmitting, setIsSubmitting] = useState(false);

    const handleToggleUtxo = (utxoId: string) => {
        const newSelection = new Set(selectedUtxos);
        if (newSelection.has(utxoId)) {
            newSelection.delete(utxoId);
        } else {
            newSelection.add(utxoId);
        }
        setSelectedUtxos(newSelection);
    };

    const handleSelectAll = () => {
        if (selectedUtxos.size === l1Utxos.length) {
            setSelectedUtxos(new Set());
        } else {
            setSelectedUtxos(new Set(l1Utxos.map(u => `${u.txHash}#${u.outputIndex}`)));
        }
    };

    const handleSubmit = async () => {
        setIsSubmitting(true);
        try {
            const utxosToCommit = l1Utxos.filter(utxo =>
                selectedUtxos.has(`${utxo.txHash}#${utxo.outputIndex}`)
            );
            await onCommit(utxosToCommit);
        } catch (error) {
            console.error("Failed to commit:", error);
        } finally {
            setIsSubmitting(false);
        }
    };

    const totalSelected = l1Utxos
        .filter(u => selectedUtxos.has(`${u.txHash}#${u.outputIndex}`))
        .reduce((sum, u) => sum + parseInt(u.amount.find(a => a.unit === 'lovelace')?.quantity || '0'), 0);

    return (
        <Modal isOpen={isOpen} onClose={onClose} title="📥 Commit UTxOs to Head">
            <p className="text-slate-400 text-sm mb-4">
                Select UTxOs from your L1 wallet to commit into the Hydra Head.
            </p>

            {/* Action Buttons */}
            <div className="flex justify-between mb-4">
                <Button
                    variant="secondary"
                    size="sm"
                    onClick={onRefresh}
                    disabled={loading}
                >
                    {loading ? '⟳ Loading...' : '⟳ Refresh'}
                </Button>
                <Button
                    variant="secondary"
                    size="sm"
                    onClick={handleSelectAll}
                >
                    {selectedUtxos.size === l1Utxos.length ? 'Deselect All' : 'Select All'}
                </Button>
            </div>

            {/* UTxO List */}
            <div className="max-h-[300px] overflow-y-auto mb-4 space-y-2">
                {loading ? (
                    <div className="text-center py-10 text-slate-500">Loading UTxOs...</div>
                ) : l1Utxos.length === 0 ? (
                    <div className="text-center py-10 text-slate-500">
                        <p>No UTxOs found at this address.</p>
                        <p className="text-sm mt-2">Make sure you have funds on the preprod testnet.</p>
                    </div>
                ) : (
                    l1Utxos.map(utxo => {
                        const utxoId = `${utxo.txHash}#${utxo.outputIndex}`;
                        const lovelace = utxo.amount.find(a => a.unit === 'lovelace')?.quantity || '0';
                        const isSelected = selectedUtxos.has(utxoId);

                        return (
                            <div
                                key={utxoId}
                                className={`
                  flex items-center gap-3 p-3 rounded-md cursor-pointer transition-all
                  bg-dark-bg border
                  ${isSelected ? 'border-accent bg-accent/10' : 'border-border hover:bg-dark-hover'}
                `}
                                onClick={() => handleToggleUtxo(utxoId)}
                            >
                                <input
                                    type="checkbox"
                                    checked={isSelected}
                                    onChange={() => handleToggleUtxo(utxoId)}
                                    onClick={e => e.stopPropagation()}
                                    className="w-4 h-4 accent-accent"
                                />
                                <div className="flex-1">
                                    <div className="text-sm text-slate-400">
                                        {utxo.txHash.slice(0, 16)}...#{utxo.outputIndex}
                                    </div>
                                    <div className="text-base text-success font-medium">
                                        {formatAda(lovelace)} ₳
                                    </div>
                                </div>
                            </div>
                        );
                    })
                )}
            </div>

            {/* Total Selected */}
            {selectedUtxos.size > 0 && (
                <div className="p-3 bg-accent/10 rounded-md text-center mb-4">
                    <strong>Total to Commit:</strong> {formatAda(totalSelected)} ₳
                </div>
            )}

            {/* Modal Actions */}
            <div className="flex justify-end gap-3">
                <Button variant="secondary" onClick={onClose}>
                    Cancel
                </Button>
                <Button
                    onClick={handleSubmit}
                    disabled={isSubmitting || selectedUtxos.size === 0}
                >
                    {isSubmitting ? 'Committing...' : `Commit (${selectedUtxos.size} UTxO${selectedUtxos.size !== 1 ? 's' : ''})`}
                </Button>
            </div>
        </Modal>
    );
};

export default CommitModal;
