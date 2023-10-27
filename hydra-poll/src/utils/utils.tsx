import {
    Data,
    Transaction,
    PlutusScript,
    UTxO,
    Recipient,
    resolvePlutusScriptAddress,
    resolveDataHash,
    Asset,
    IInitiator,
    Protocol
} from '@meshsdk/core'
import { metadataLabel } from "../types/types"

export const buildScriptTx = async (
    code: string,
    txHash: string,
    voteOption: number,
    initiator: IInitiator,
    parameters: Protocol,
    networkId?: number
) => {
    const script: PlutusScript = {
        version: "V2",
        code
    }
    const scriptAddress = resolvePlutusScriptAddress(script, networkId)
    const datumValue = "d87980"
    const dataHash = resolveDataHash(datumValue)
    const asset: Asset = {
        unit: "lovelace",
        quantity: "98000000"
    }
    const value: UTxO = {
        input: {
            outputIndex: 0,
            txHash
        },
        output: {
            address: scriptAddress,
            amount: [asset],
            dataHash,
        }
    }
    const data: Data = { alternative: 0, fields: [] }
    const recipient: Recipient = {
        address: scriptAddress,
        datum: { value: data }
    }
    const tx = new Transaction({ initiator, parameters })
        .redeemValue({
            value,
            script,
            datum: data,
            redeemer: data
        })
        .sendLovelace(recipient, asset.quantity)
        .setCollateral([value])
        .setTxInputs([]) // hack to prevent the builder from trying to balance the tx
        .setMetadata(metadataLabel, voteOption)

    const unsignedTx = await tx.build()
    return unsignedTx
}
