import Head from "next/head"

import Image from "next/image"
import IntervalSettingProvider from "@/providers/IntervalProvider"
import IntervalSetter from "@/components/IntervalSetter"
import TickBox from "@/components/TickBox"
import HeadsTable from "@/components/HeadsTable"
import { HeadsDataProvider } from "@/providers/HeadsDataProvider"


export default function Home() {
  return (
    <main className="items-center">
      <div className="">
        <Head>
          <title>Hydrascan</title>
          <meta name="description" content="Hydra Head Explorer" />
        </Head>

        <IntervalSettingProvider>

          <div className="flex flex-col items-center justify-center h-screen">
            <h1 className="text-3xl font-bold mb-4 flex items-center">
              <div className="mr-2">
                <Image
                  src="/hydra.svg"
                  alt="Hydra Logo"
                  className="dark:invert"
                  width={100}
                  height={24}
                  priority
                />
              </div>
              Hydrascan
            </h1>

            <div className="flex items-start space-x-4">
              <div>
                <TickBox />
              </div>
              <div>
                <IntervalSetter />
              </div>

            </div>
            <HeadsDataProvider>
              <HeadsTable />
            </HeadsDataProvider>
          </div>


        </IntervalSettingProvider>
      </div>
    </main>
  )
}
