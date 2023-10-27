import { useState } from "react"
import { decode } from 'cbor-x/decode'
import { Option, metadataLabel } from '../../types/types'
import Poll from "./poll"
import { useHydraEvent } from "../../lib/hydra-ws/hook"
import {
    HydraEventType,
    HydraEvent,
    ServerOutputTag,
} from "../../lib/hydra-ws/model/events"

export default function Main() {
    const [options, setOptions] = useState<Option[]>([
        { id: 1, text: 'Incremental commits/decommits', votes: 0 },
        { id: 2, text: 'Dynamic Hydra Parties', votes: 0 },
        { id: 3, text: 'Interconnected Hydra Heads', votes: 0 },
    ])
    const [txHash, setTxHash] = useState<string>("")

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
        // console.log("HydraEvent: %o", event.tag)
        switch (event.tag) {
            case HydraEventType.Update:
                switch (event.output.tag) {
                    case ServerOutputTag.HeadIsOpen:
                        const headIsOpen = event.output
                        const txId = Object.keys(headIsOpen.utxo)[0].split('#')[0]
                        setTxHash(txId)
                        break
                    case ServerOutputTag.TxValid:
                        const txValid = event.output
                        setTxHash(txValid.transaction.id)
                        if (txValid.transaction.auxiliaryData != null) {
                            const hex = Buffer.from(txValid.transaction.auxiliaryData, 'hex')
                            const aux = decode(hex)
                            const voteOption = aux[metadataLabel]
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
            <Poll options={options} txHash={txHash} />
        </div>
    )
}
