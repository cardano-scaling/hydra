import libtmux
import os
import subprocess
import sys
import time


if not os.path.exists('demo'):
    raise Exception(
        'Plase navigate to the root directory of the hydra project'
    )

os.chdir('demo')

server = libtmux.Server()

SESSION_NAME = 'hydra-demo'
session = server.find_where({'session_name': SESSION_NAME})


def send_cmd(pane, cmd):
    pane.send_keys(cmd)
    time.sleep(1)
    pane.enter()


if not session:
    session = server.new_session(session_name=SESSION_NAME)

    subprocess.run('[ -d devnet ] || ./prepare-devnet.sh', shell=True)

    # Let's improve bootstraper experience by live loading
    # pieces of the tmux session. We do this by forking out
    # the bootstrapping process.
    if os.fork() <= 0:
        [cardano_node_window] = session.list_windows()
        cardano_node_window.rename_window('cardano-node')
        [cardano_node_pane] = cardano_node_window.list_panes()

        send_cmd(cardano_node_pane, ' '.join([
            'cardano-node', 'run',
            '--config', 'devnet/cardano-node.json',
            '--topology', 'devnet/topology.json',
            '--database-path', 'devnet/db',
            '--socket-path', 'devnet/node.socket',
            '--shelley-operational-certificate', 'devnet/opcert.cert',
            '--shelley-kes-key', 'devnet/kes.skey',
            '--shelley-vrf-key', 'devnet/vrf.skey'
        ]))
        time.sleep(2)
        hydra_node_pane_seed = cardano_node_window.split_window(
            vertical=True)
        send_cmd(
            hydra_node_pane_seed,
            'export CARDANO_NODE_SOCKET_PATH="devnet/node.socket"')
        if os.path.exists('.env'):
            os.unlink('.env')
        send_cmd(
            hydra_node_pane_seed,
            './seed-devnet.sh $(which cardano-cli) $(which hydra-node)')
        while True:
            if os.path.exists('.env'):
                break
            time.sleep(1)

        hydra_nodes_window = session.new_window(
            window_name='hydra-nodes', attach=False)

        [hydra_node_pane_alice] = hydra_nodes_window.list_panes()
        send_cmd(hydra_node_pane_alice, ' '.join([
            'source .env', '&&',
            'hydra-node',
            '--node-id', '1',
            '--listen', '127.0.0.1:5001',
            '--api-port', '4001',
            '--monitoring-port', '6001',
            '--peer', '127.0.0.1:5002',
            '--peer', '127.0.0.1:5003',
            '--hydra-signing-key', 'alice.sk',
            '--hydra-verification-key', 'bob.vk',
            '--hydra-verification-key', 'carol.vk',
            '--hydra-scripts-tx-id',  '$HYDRA_SCRIPTS_TX_ID',
            '--cardano-signing-key', 'devnet/credentials/alice.sk',
            '--cardano-verification-key', 'devnet/credentials/bob.vk',
            '--cardano-verification-key', 'devnet/credentials/carol.vk',
            '--ledger-protocol-parameters', 'devnet/protocol-parameters.json',
            '--testnet-magic', '42',
            '--node-socket', 'devnet/node.socket',
            ]))
        hydra_node_pane_bob = hydra_nodes_window.split_window(vertical=True)
        send_cmd(hydra_node_pane_bob, ' '.join([
            'source .env', '&&',
            'hydra-node',
            '--node-id', '2',
            '--listen', '127.0.0.1:5002',
            '--api-port', '4002',
            '--monitoring-port', '6002',
            '--peer', '127.0.0.1:5001',
            '--peer', '127.0.0.1:5003',
            '--hydra-signing-key', 'bob.sk',
            '--hydra-verification-key', './alice.vk',
            '--hydra-verification-key', './carol.vk',
            '--hydra-scripts-tx-id',  '$HYDRA_SCRIPTS_TX_ID',
            '--cardano-signing-key', 'devnet/credentials/bob.sk',
            '--cardano-verification-key', 'devnet/credentials/alice.vk',
            '--cardano-verification-key', 'devnet/credentials/carol.vk',
            '--ledger-protocol-parameters', 'devnet/protocol-parameters.json',
            '--testnet-magic', '42',
            '--node-socket', 'devnet/node.socket',
            ]))
        hydra_node_pane_carol = hydra_nodes_window.split_window(vertical=True)
        send_cmd(hydra_node_pane_carol, ' '.join([
            'source .env', '&&',
            'hydra-node',
            '--node-id', '3',
            '--listen', '127.0.0.1:5003',
            '--api-port', '4003',
            '--monitoring-port', '6003',
            '--peer', '127.0.0.1:5001',
            '--peer', '127.0.0.1:5002',
            '--hydra-signing-key', 'carol.sk',
            '--hydra-verification-key', './alice.vk',
            '--hydra-verification-key', './bob.vk',
            '--hydra-scripts-tx-id',  '$HYDRA_SCRIPTS_TX_ID',
            '--cardano-signing-key', 'devnet/credentials/carol.sk',
            '--cardano-verification-key', 'devnet/credentials/alice.vk',
            '--cardano-verification-key', 'devnet/credentials/bob.vk',
            '--ledger-protocol-parameters', 'devnet/protocol-parameters.json',
            '--testnet-magic', '42',
            '--node-socket', 'devnet/node.socket',
            ]))
        time.sleep(2)

        hydra_tuis_window = session.new_window(
            window_name='hydra-tuis', attach=False)

        [hydra_tui_pane_alice] = hydra_tuis_window.list_panes()
        send_cmd(hydra_tui_pane_alice, ' '.join([
             'hydra-tui',
             '--connect', '0.0.0.0:4001',
             '--cardano-signing-key', 'devnet/credentials/alice.sk',
             '--testnet-magic', '42',
             '--node-socket', 'devnet/node.socket',
            ]))

        hydra_tui_pane_bob = hydra_tuis_window.split_window(vertical=True)
        send_cmd(hydra_tui_pane_bob, ' '.join([
            'hydra-tui',
            '--connect', '0.0.0.0:4002',
            '--cardano-signing-key', 'devnet/credentials/bob.sk',
            '--testnet-magic', '42',
            '--node-socket', 'devnet/node.socket',
            ]))

        hydra_tui_pane_carol = hydra_tuis_window.split_window(vertical=True)
        send_cmd(hydra_tui_pane_carol, ' '.join([
            'hydra-tui',
            '--connect', '0.0.0.0:4003',
            '--cardano-signing-key', 'devnet/credentials/carol.sk',
            '--testnet-magic', '42',
            '--node-socket', 'devnet/node.socket',
            ]))
        sys.exit(0)

server.attach_session(SESSION_NAME)
