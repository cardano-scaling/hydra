// import { CardanoWallet, MeshBadge } from "@meshsdk/react";
import Poll from "./poll/poll";
import { useState } from "react";
import HydraEventType, { HydraEvent, TxValid } from "../lib/model/events";
import useHydraEvent from "./hydra-ws/hook";
import { decode } from 'cbor-x/decode' 
import Option from '../lib/model/state' 

export default function Home() {
  // const [state, setState] = useState(transitions.disconnected(options))
  const [options, setOptions] = useState<Option[]>([
    { id: 1, text: 'Incremental commits/decommits', votes: 0 },
    { id: 2, text: 'Dynamic Hydra Parties', votes: 0 },
    { id: 3, text: 'Interconnected Hydra Heads', votes: 0 },
  ])

  const updateVoteCount = (optionId: number) => {
    setOptions((prevOptions) =>
      prevOptions.map((option) =>
        option.id === optionId
          ? { ...option, votes: option.votes + 1 }
          : option
      )
    )
  }

  useHydraEvent((event: HydraEvent) => {
    // console.log("HydraEvent: %o", event)
    switch (event.tag) {
      case HydraEventType.ClientConnected:
        // setState(transitions.connected(options))
        break
      case HydraEventType.ClientDisconnected:
        // setState(transitions.disconnected(options))
        break
      case HydraEventType.Update:
        // setState(transitions.handleAppEvent(state, event.output))
        switch (event.output.tag) {
          case "TxValid":
            const txValid = event.output as TxValid
            const metadataLabel = 14

            if (txValid.transaction.auxiliaryData != null) {
              const aux = decode(Buffer.from(txValid.transaction.auxiliaryData, 'hex'))
              const voteOption = aux.get(0)[metadataLabel]
              updateVoteCount(voteOption)
            }
            break
          default:
            break
        }
        break
      default:
        break
    }
  })

  return (
    <div className="container">
      <header className="title">
        <title>Hydra Poll</title>
        <meta name="description" content="Poll running on Hydra Head protocol" />
      </header>

      <main className="main">
        <Poll options={options} />
      </main>

      <footer className="footer">
      </footer>
    </div>
  )
}
