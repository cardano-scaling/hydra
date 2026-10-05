import Layout from "@theme/Layout";
import Hero from "../components/homepage/Hero";

import { forLaptop } from "../../helpers/media-queries";
import useMediaQuery from "../hooks/useMediaQuery";

import { PageContext, PageType } from "../context/PageContext";

export default function Home() {
  const isLaptopUp = useMediaQuery(forLaptop);
  return (
    <PageContext.Provider value={{ page: PageType.Landing }}>
      <div className="z-index:1000">
        <Layout>
          <Hero />
        </Layout>
      </div>
    </PageContext.Provider>
  );
}
