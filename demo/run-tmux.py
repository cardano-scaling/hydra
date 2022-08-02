''
# The above quotation is required because we import this
# module as a nix `String`.
# Please don't use triple quotes inside this `Python` module.

import libtmux
from collections import namedtuple
import os
import subprocess
import time

if not os.path.exists('demo'):
    raise Exception(
        'Plase navigate to the root directory of the hydra-poc project'
    )

root_dir = os.getcwd()
demo_dir = os.path.join(root_dir, 'demo')
devnet_dir = os.path.join(demo_dir, 'devnet')

os.chdir(demo_dir)

if not os.path.exists(devnet_dir):
    subprocess.run('./prepare-devnet.sh')
    subprocess.run('./seed-devnet.sh $(which cardano-cli)')

SESSION_NAME = 'hydra-demo'


PaneCfg = namedtuple('PaneCfg', ['cmd', 'await_cmd'])

WindowCfg = namedtuple('WindowCfg', ['name', 'panes'])


# A placeholder for proper "await command" checker which verifies
# subprocess readiness like: `$ cardano-cli query tip...`
def await_nseconds(seconds):
    def _sleep():
        time.sleep(seconds)
        return True
    return _sleep


cardano_node_cmd = ' '.join([
    'cardano-node run',
    '--config devnet/cardano-node.json',
    '--topology devnet/topology.json',
    '--database-path devnet/db',
    '--socket-path devnet/ipc/node.socket',
    '--shelley-operational-certificate devnet/opcert.cert',
    '--shelley-kes-key devnet/kes.skey',
    '--shelley-vrf-key devnet/vrf.skey',
])
cardano_node_window_cfg = WindowCfg(
    'cardano-node',
    [PaneCfg(
        cardano_node_cmd,
        await_nseconds(1)
        # FIXME: We should probably use some more reliable strategy here like:
        # await_command_succeeds(
        #    'cardano-cli query tip --testnet-magic 42', 10
        # )
    )]
)

NodeCfg = namedtuple('NodeCfg', ['id', 'port', 'api_port', 'monitoring_port'])

hydras_nodes_cfg = {
    'alice': NodeCfg(1, 5001, 4001, 6001),
    'bob': NodeCfg(2, 5002, 4002, 6002),
    'carol': NodeCfg(3, 5003, 4003, 6003)
}


def hydra_node_cmd(party):
    node_cfg = hydras_nodes_cfg[party]
    peers = [k for k in hydras_nodes_cfg.keys() if k != party]

    cmd = [
      'hydra-node',
      '--node-id %i' % node_cfg.id,
      '--port %s' % node_cfg.port,
      '--api-port %s' % node_cfg.api_port,
      '--monitoring-port %s' % node_cfg.monitoring_port,
      '--hydra-signing-key %s.sk' % party,
      '--cardano-signing-key devnet/credentials/%s.sk' % party,
      '--ledger-genesis devnet/genesis-shelley.json',
      '--ledger-protocol-parameters devnet/protocol-parameters.json',
      '--network-id 42',
      '--node-socket devnet/ipc/node.socket',
    ]

    peers_hosts = [
        '--peer localhost:%s' % hydras_nodes_cfg[peer].port for peer in peers
    ]
    hydra_verification_keys = [
        ('--hydra-verification-key %s.vk' % peer)
        for peer in peers
    ]
    cardano_verification_keys = [
        ('--cardano-verification-key devnet/credentials/%s.vk' % peer)
        for peer in peers
    ]

    return ' '.join(
        cmd +
        peers_hosts +
        hydra_verification_keys +
        cardano_verification_keys
    )


def hydra_node_pane_cfg(party):
    cmd = hydra_node_cmd(party)
    return PaneCfg(cmd, await_nseconds(1))


def hydra_tui_pane_cfg(party):
    cmd = ' '.join([
        'hydra-tui',
        '--connect 0.0.0.0:%s' % hydras_nodes_cfg[party].api_port,
        '--cardano-signing-key devnet/credentials/%s.sk' % party,
        '--network-id 42',
        '--node-socket devnet/ipc/node.socket',
    ])
    return PaneCfg(cmd, lambda: True)


hydra_node_window_cfg = WindowCfg(
    'hydra-nodes',
    [hydra_node_pane_cfg(k) for k in hydras_nodes_cfg.keys()]
)
hydra_tuis_window_cfg = WindowCfg(
    'hydra-tuis',
    [hydra_tui_pane_cfg(k) for k in hydras_nodes_cfg.keys()]
)

server = libtmux.Server()

session = server.find_where({'session_name': SESSION_NAME})


def setup_window(window_cfg, window=None):
    if window_cfg.panes:
        if window is None:
            window = session.new_window(
                window_name=window_cfg.name,
                attach=False
            )

        [pane] = window.list_panes()
        pane_cfg = window_cfg.panes[0]
        pane.send_keys(pane_cfg.cmd)
        time.sleep(1)
        pane.enter()
        pane_cfg.await_cmd()

        for pane_cfg in window_cfg.panes[1:]:
            pane = window.split_window(vertical=True)
            pane.send_keys(pane_cfg.cmd)
            # Please don't ask me why we need to `sleep`
            # before we can perform working `enter`...
            time.sleep(1)
            pane.enter()
            pane_cfg.await_cmd()
    return window


hydra_tuis_window = None

child = False
if not session:
    session = server.new_session(session_name=SESSION_NAME)
    time.sleep(1)
    pid = os.fork()
    if pid <= 0:
        child = True
        [window] = session.list_windows()
        setup_window(cardano_node_window_cfg, window=window)
        setup_window(hydra_node_window_cfg)
        hydra_tuis_window = setup_window(hydra_tuis_window_cfg)


if not child:
    server.attach_session(SESSION_NAME)
    # We attach to the tuis window at the end because interaction
    # between curses and tmux breaks the UI sometimes and redraw
    # is required to fix the situation (like a window switch).
    if hydra_tuis_window:
        hydra_tuis_window.select_window()
''
