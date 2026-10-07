import Layout from "@theme/Layout";
import Hero from "../components/homepage/Hero";

import { forLaptop } from "../../helpers/media-queries";
import useMediaQuery from "../hooks/useMediaQuery";

import { PageContext, PageType } from "../context/PageContext";
import WhyHydraHead from "../components/homepage/WhyHydraHead";
import ProtocolFeatures from "../components/homepage/ProtocolFeatures";
import WhatIsHydra from "../components/homepage/WhatIsHydra";
import Topologies from "../components/homepage/Topologies";
import FastAndFrequentActivity from "../components/homepage/FastAndFrequentActivity";

export default function Home() {
  const isLaptopUp = useMediaQuery(forLaptop);
  return (
    <PageContext.Provider value={{ page: PageType.Landing }}>
      <div className="z-index:1000">
        <Layout>
          <Hero />
          <WhyHydraHead />
          <WhatIsHydra />
          <ProtocolFeatures />
          <Topologies />
          <FastAndFrequentActivity />
        </Layout>
      </div>
    </PageContext.Provider>
  );
}
