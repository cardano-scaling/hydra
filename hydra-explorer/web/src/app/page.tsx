import Head from "next/head";
import HeadsTable from "./headsTable";

export default function Home() {
  return (
    <main className="items-center">
      <div className="">
        <Head>
          <title>Hydrascan</title>
          <meta name="description" content="Hydra Head Explorer" />
        </Head>
      </div>

      <div className="">
        <HeadsTable />
      </div>
    </main>
  );
}
