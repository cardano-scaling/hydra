import React from 'react';
import { Card, CardHeader, Button, Input, StatusBadge } from './ui';

type ConnectionStatus = 'disconnected' | 'connecting' | 'connected';

interface NodeConnectionProps {
    nodeUrl: string;
    status: ConnectionStatus;
    onUrlChange: (url: string) => void;
    onConnect: () => void;
    onDisconnect: () => void;
}

export const NodeConnection: React.FC<NodeConnectionProps> = ({
    nodeUrl,
    status,
    onUrlChange,
    onConnect,
    onDisconnect,
}) => {
    return (
        <Card>
            <CardHeader>🔌 Hydra Node</CardHeader>

            {/* WebSocket URL */}
            <div className="mb-4">
                <Input
                    label="WebSocket URL"
                    type="text"
                    value={nodeUrl}
                    onChange={(e) => onUrlChange(e.target.value)}
                    placeholder="ws://localhost:4001"
                    disabled={status === 'connected'}
                />
            </div>

            {/* Connect/Disconnect Buttons */}
            <div className="flex gap-2 mb-3">
                <Button
                    onClick={onConnect}
                    disabled={status === 'connected' || status === 'connecting' || !nodeUrl.trim()}
                >
                    {status === 'connecting' ? 'Connecting...' : 'Connect'}
                </Button>
                <Button
                    variant="secondary"
                    onClick={onDisconnect}
                    disabled={status !== 'connected'}
                >
                    Disconnect
                </Button>
            </div>

            {/* Status */}
            <div className="text-sm text-slate-400 flex items-center gap-2">
                Status: <StatusBadge status={status} />
            </div>
        </Card>
    );
};
