import React from "react";
import Layout from "@theme/Layout";
import { translate } from "@docusaurus/Translate";
import Features from "../components/homepage/Features";
import Description from "../components/homepage/Description";
import HowItWorks from "../components/homepage/HowItWorks";
import Carousel from "../components/homepage/Carousel/Coursel";
import FeaturesFAQ from "../components/homepage/FeaturesFAQ";
import CaseStudies from "../components/homepage/CaseStudies";
import { forLaptop } from "../../helpers/media-queries";
import useMediaQuery from "../hooks/useMediaQuery";
import ResponsiveCarousel from "../components/homepage/Carousel/ResponsiveCarousel";
import HomepageHero from "../components/homepage/HomepageHero";

export default function Home() {
  const isLaptopUp = useMediaQuery(forLaptop);
  return (
    <Layout
      title={""}
      description={`${translate({
        id: "homepage.tagline",
        message: "User manual, core concepts, API reference",
      })}`}
    >
      <HomepageHero />
      <main>
        <Features />
        <div className="pageContainer">
          <Description />
          <HowItWorks />
        </div>
        {isLaptopUp ? <Carousel /> : <ResponsiveCarousel />}
        <FeaturesFAQ />
        <CaseStudies />
      </main>
    </Layout>
  );
}
