import Head from "next/head"
import Main from "./poll/main"

export default function Home() {
  return (
    <div className="container">
      <Head>
        <title>Hydra Poll</title>
        <meta name="description" content="Poll running on Hydra Head protocol" />
      </Head>

      <main className="main">
        <Main />
      </main>

      <footer className="footer">
      </footer>
    </div>
  )
}
