import React, { useContext } from 'react'
import { useNetwork, useWallet } from '@meshsdk/react'
import { Protocol } from '@meshsdk/core'
import { DEFAULT_PROTOCOL_PARAMETERS, Option } from "../../types/types"
import { HydraSocketContext } from '../../lib/hydra-ws/context'
import { buildScriptTx } from '../../utils/utils'

const Poll: React.FC<{ options: Option[], txHash: string }> = ({ options, txHash }) => {
    const { socket } = useContext(HydraSocketContext)
    const { wallet, connected } = useWallet()
    const network = useNetwork()

    const code = process.env.PLUTUS_SCRIPT_CODE || "default_script_code"

    const parameters: Protocol = {
        ...DEFAULT_PROTOCOL_PARAMETERS,
        minFeeA: 0,
        minFeeB: 0,
        priceMem: 0,
        priceStep: 0,
        collateralPercent: 0,
        coinsPerUTxOSize: '0'
    }

    // Function to send a vote message through WebSocket
    const handleVote = async (voteOption: number) => {
        if (connected) {
            const unsignedTx = await buildScriptTx(code, txHash, voteOption, wallet, parameters, network)
            const signedTx = await wallet.signTx(unsignedTx, true)
            const newTx = { "tag": "NewTx", "transaction": signedTx.toString() }
            const message = JSON.stringify(newTx)
            socket.send(message)
        } else {
            console.error("Cant build tx due to missing Lucid instance")
        }
    }

    return (
        <div>
            <div>
                <h1 className="title">
                    <a href="https://hydra.family">Hydra </a>Poll
                </h1>
                <h2>Vote for the next Hydra feature</h2>
            </div>
            <div className='container'>
                <div className="grid">
                    {options.map((option) => (
                        <div className="card" key={option.id}>
                            <h3>{option.text}</h3>
                            <h4>{option.votes}</h4>
                            <button className="button" onClick={() =>
                                handleVote(option.id).catch((error) => {
                                    console.error("Error on handleVote:", error)
                                })
                            }>Vote</button>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    )
}

export default Poll
