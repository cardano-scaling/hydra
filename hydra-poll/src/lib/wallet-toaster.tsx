import { useEffect, useState } from 'react'
import { useAddress, useWallet } from '@meshsdk/react'
import { toast } from 'react-hot-toast'

export default function useNotification() {
    const { connected, connecting } = useWallet()
    const address = useAddress()
    const [isNotConnected, setIsNotConnected] = useState(true)

    const formatAddressWithPrefix = (address: string): string => {
        const prefix = "addr_test"
        const prefixLength = prefix.length
        const suffixLength = 5

        const prefixPart = address.substring(0, prefixLength + suffixLength)
        const suffixPart = address.substring(address.length - suffixLength)

        return `${prefixPart}...${suffixPart}`
    }

    useEffect(() => {
        if (connected && address !== undefined) {
            const message = `Wallet Connected\n${formatAddressWithPrefix(address)}`
            toast.success(message, {
                icon: '🚀',
                duration: 2000,
                position: 'top-right',
                style: {
                    background: 'green',
                    fontWeight: 350,
                    color: 'white'
                },
            })
            setIsNotConnected(false)
        }
        if (!connected && !isNotConnected) {
            toast.error('Wallet Disconnected', {
                icon: '👋',
                duration: 1500,
                position: 'top-right',
                style: {
                    background: 'red',
                    fontWeight: 350,
                    color: 'white'
                },
            })
            setIsNotConnected(true)
        }
    }, [connecting, connected, address])
}
