import React from "react";
import Layout from "@theme/Layout";
import Features from "../components/homepage/Features";
import AnimatedText from "../components/homepage/AnimatedText";
import HowItWorks from "../components/homepage/HowItWorks";
import Carousel from "../components/homepage/Carousel/Carousel";
import FeaturesFAQ from "../components/homepage/FeaturesFAQ";
import CaseStudies from "../components/homepage/CaseStudies";
import { forLaptop } from "../../helpers/media-queries";
import useMediaQuery from "../hooks/useMediaQuery";
import ResponsiveCarousel from "../components/homepage/Carousel/ResponsiveCarousel";
import HomepageHero from "../components/homepage/HomepageHero";

export default function Home() {
  const isLaptopUp = useMediaQuery(forLaptop);
  return (
    <div className="z-index:1000">
      <Layout>
        <HomepageHero />
        <main>
          <Features />
          <div className="pageContainer">
            <AnimatedText />
            <HowItWorks />
          </div>
          {isLaptopUp ? <Carousel /> : <ResponsiveCarousel />}
          <FeaturesFAQ />
          <CaseStudies />
        </main>
      </Layout>
    </div>
  );
}
