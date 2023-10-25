import Head from "next/head"
import Main from "./poll/main"
import { CardanoWallet } from "@meshsdk/react"
import { Toaster } from "react-hot-toast"
import useWalletToaster from '../lib/wallet-toaster'

export default function Home() {
  useWalletToaster()

  return (
    <div className="container">
      <Head>
        <title>Hydra Poll</title>
        <meta name="description" content="Poll running on Hydra Head protocol" />
      </Head>

      <CardanoWallet />

      <main className="main">
        <Main />
      </main>

      <Toaster />

      <footer className="footer">
      </footer>
    </div>
  )
}
