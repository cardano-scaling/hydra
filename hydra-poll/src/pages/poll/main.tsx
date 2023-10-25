import { useState } from "react"
import { HydraEventType, HydraEvent, ServerOutputTag } from "../../lib/hydra-ws/model/events"
import useHydraEvent from "../../lib/hydra-ws/hook"
import { decode } from 'cbor-x/decode'
import Option from '../../types/option'
import Poll from "./poll"

export default function Main() {
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
            case HydraEventType.Update:
                // setState(transitions.handleAppEvent(state, event.output))
                switch (event.output.tag) {
                    case ServerOutputTag.TxValid:
                        const txValid = event.output
                        const metadataLabel = 14
                        if (txValid.transaction.auxiliaryData != null) {
                            const hex = Buffer.from(txValid.transaction.auxiliaryData, 'hex')
                            const aux = decode(hex)
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
        <div>
            <Poll options={options} />
        </div>
    )
}
