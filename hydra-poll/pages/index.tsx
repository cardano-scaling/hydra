import Head from "next/head";
import { CardanoWallet, MeshBadge } from "@meshsdk/react";
// import {Poll} from "pages/poll/Poll";

export default function Home() {
  return (
    <div className="container">
      <Head>
        <title>Hydra Poll</title>
        <meta name="description" content="Poll running on Hydra Head protocol" />
        <link
          rel="icon"
          href="https://meshjs.dev/favicon/favicon-32x32.png"
        />
        <link
          href="https://meshjs.dev/css/template.css"
          rel="stylesheet"
          key="mesh-demo"
        />
      </Head>

      <main className="main">
        <h1 className="title">
          <a href="https://hydra.family">Hydra</a> Poll
        </h1>

        <div className="grid">
        </div>
      </main>

      <footer className="footer">
        <MeshBadge dark={true} />
      </footer>
    </div>
  );
}
