import "../../styles/globals.css"
import type { AppProps } from "next/app"
import { MeshProvider } from "@meshsdk/react"
import dynamic from 'next/dynamic'

const HydraSocketProviderNoSSR = dynamic(() => import("../lib/hydra-ws/provider"), {
    ssr: false,
})

const UserHydraSocketOptions = {
  // FIXME: .env file is not supported
  // url: process.env.REACT_APP_HYDRA_NODE_URL || "default_url"
  url: "ws://13.39.230.205:4001"
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
