import libtmux
import os
import subprocess
import sys
import time


# Run from the repository root so devnet/, demo/ and delegated-demo/ resolve.
if not (os.path.isdir('demo') and os.path.isdir('delegated-demo')):
    raise Exception('Please run from the root directory of the hydra project')

server = libtmux.Server()

SESSION_NAME = 'delegated-demo'
session = server.find_where({'session_name': SESSION_NAME})

MEDIATORS = ['alice', 'bob', 'carol']


def send_cmd(pane, cmd):
    pane.send_keys(cmd)
    time.sleep(1)
    pane.enter()


def hydra_node(node_id, name, api, listen, mon, peers):
    others = [n for n in MEDIATORS if n != name]
    args = [
        'source .env', '&&', 'hydra-node',
        '--node-id', str(node_id),
        '--listen', '127.0.0.1:%d' % listen,
        '--api-port', str(api),
        '--monitoring-port', str(mon),
    ]
    for p in peers:
        args += ['--peer', '127.0.0.1:%d' % p]
    args += ['--hydra-signing-key', 'demo/%s.sk' % name]
    for other in others:
        args += ['--hydra-verification-key', 'demo/%s.vk' % other]
    args += [
        '--hydra-scripts-tx-id', '$HYDRA_SCRIPTS_TX_ID',
        '--cardano-signing-key', 'devnet/credentials/%s.sk' % name,
    ]
    for other in others:
        args += ['--cardano-verification-key',
                 'devnet/credentials/%s.vk' % other]
    args += [
        '--ledger-protocol-parameters', 'devnet/protocol-parameters.json',
        '--persistence-dir', 'devnet/persistence/%s' % name,
        '--contestation-period', '3s',
        # High deposit-period keeps the deposit tx validity window wide enough
        # (~20s) to sign+submit an external commit with cardano-cli on this
        # 0.1s-slot devnet; see nix/hydra/delegated-demo.nix for details.
        '--deposit-period', '500s',
        '--deposit-activation', '10s',
        '--testnet-magic', '42',
        '--node-socket', 'devnet/node.socket',
    ]
    return ' '.join(args)


if not session:
    session = server.new_session(session_name=SESSION_NAME)

    subprocess.run('[ -d devnet ] || ./demo/prepare-devnet.sh', shell=True)

    # Fork out the bootstrapper so panes populate live while we attach.
    if os.fork() <= 0:
        [cardano_window] = session.list_windows()
        cardano_window.rename_window('cardano-node')
        [cardano_pane] = cardano_window.list_panes()
        send_cmd(cardano_pane, ' '.join([
            'cardano-node', 'run',
            '--config', 'devnet/cardano-node.json',
            '--topology', 'devnet/topology.json',
            '--database-path', 'devnet/db',
            '--socket-path', 'devnet/node.socket',
            '--shelley-operational-certificate', 'devnet/opcert.cert',
            '--shelley-kes-key', 'devnet/kes.skey',
            '--shelley-vrf-key', 'devnet/vrf.skey',
        ]))
        time.sleep(2)

        seed_pane = cardano_window.split_window(vertical=True)
        send_cmd(seed_pane,
                 'export CARDANO_NODE_SOCKET_PATH="devnet/node.socket"')
        if os.path.exists('.env'):
            os.unlink('.env')
        send_cmd(seed_pane, './delegated-demo/seed-devnet.sh')
        while not os.path.exists('.env'):
            time.sleep(1)

        mediators = session.new_window(window_name='mediators', attach=False)
        [alice_pane] = mediators.list_panes()
        send_cmd(alice_pane,
                 hydra_node(1, 'alice', 4001, 5001, 6001, [5002, 5003]))
        bob_pane = mediators.split_window(vertical=True)
        send_cmd(bob_pane,
                 hydra_node(2, 'bob', 4002, 5002, 6002, [5001, 5003]))
        carol_pane = mediators.split_window(vertical=True)
        send_cmd(carol_pane,
                 hydra_node(3, 'carol', 4003, 5003, 6003, [5001, 5002]))
        mediators.select_layout('even-vertical')
        time.sleep(2)

        actors = session.new_window(window_name='actors', attach=False)
        [anna_pane] = actors.list_panes()
        send_cmd(anna_pane, './delegated-demo/actor.sh anna 4001')
        elsa_pane = actors.split_window(vertical=False)
        send_cmd(elsa_pane, './delegated-demo/actor.sh elsa 4002')
        observer_pane = actors.split_window(vertical=True)
        send_cmd(observer_pane, './delegated-demo/observe.sh')
        actors.select_layout('tiled')
        sys.exit(0)

server.attach_session(SESSION_NAME)
