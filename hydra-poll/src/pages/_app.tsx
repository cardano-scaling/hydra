import "../../styles/globals.css"
import type { AppProps } from "next/app"
import { MeshProvider } from "@meshsdk/react"
import dynamic from 'next/dynamic'

const HydraSocketProviderNoSSR = dynamic(() => import("../lib/hydra-ws/provider"), {
  ssr: false,
})

const UserHydraSocketOptions = {
  url: process.env.REACT_APP_HYDRA_NODE_URL || "default_url"
}

export default function App({ Component, pageProps }: AppProps) {
  return (
    <MeshProvider>
      <HydraSocketProviderNoSSR {...UserHydraSocketOptions}>
        <Component {...pageProps} />
      </HydraSocketProviderNoSSR>
    </MeshProvider>
  )
}
