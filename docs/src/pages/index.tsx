import React from "react";
import Layout from "@theme/Layout";
import Features from "../components/homepage/Features";
import AnimatedText from "../components/homepage/AnimatedText";
import WhyHydraHead from "../components/homepage/WhyHydraHead";
import Carousel from "../components/homepage/Carousel/Carousel";
import Properties from "../components/homepage/Properties";
import CaseStudies from "../components/homepage/CaseStudies";
import { forLaptop } from "../../helpers/media-queries";
import useMediaQuery from "../hooks/useMediaQuery";
import ResponsiveCarousel from "../components/homepage/Carousel/ResponsiveCarousel";
import HomepageHero from "../components/homepage/HomepageHero";
import { PageContext, PageType } from "../context/PageContext";

export default function Home() {
  const isLaptopUp = useMediaQuery(forLaptop);
  return (
    <PageContext.Provider value={{ page: PageType.Landing }}>
      <div className="z-index:1000">
        <Layout>
          <HomepageHero />
          <main className="bg-white">
            <Features />
            <div className="pageContainer">
              <AnimatedText />
              <WhyHydraHead />
            </div>
            {isLaptopUp ? <Carousel /> : <ResponsiveCarousel />}
            <Properties />
            <CaseStudies />
          </main>
        </Layout>
      </div>
    </PageContext.Provider>
  );
}
