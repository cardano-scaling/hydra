import "../../styles/globals.css";
import type { AppProps } from "next/app";
import { MeshProvider } from "@meshsdk/react";
import HydraSocketProvider from "./hydra-ws/provider";

const UserHydraSocketOptions = {
  url: process.env.REACT_APP_HYDRA_NODE_URL || "default_url"
}

export default function App({ Component, pageProps }: AppProps) {
  return (
    <MeshProvider>
      <HydraSocketProvider {...UserHydraSocketOptions}>
        <Component {...pageProps} />
      </HydraSocketProvider>
    </MeshProvider>
  );
}
