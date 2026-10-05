import React, { FC } from "react";
import Link from "@docusaurus/Link";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import { forTablet } from "../../../helpers/media-queries";
import { motion } from "framer-motion";
import { HomepageHeroContent } from "../../../docs/homepage/homepage-hero";

const HomepageHero: FC = () => {
  const { siteConfig } = useDocusaurusContext();
  return (
    <div className="relative bg-[url('../../static/desktop-video-still.jpg')] bg-cover z-20 -mt-[var(--ifm-navbar-height)] pt-[var(--ifm-navbar-height)]">
      <video
        autoPlay
        muted
        loop
        playsInline
        preload="auto"
        onPlaying={(e) => {
          (e.target as HTMLVideoElement).style.opacity = "1";
        }}
        aria-label="Video background"
        className="absolute top-0 left-0 right-0 bottom-0 object-cover -z-10 w-full h-full opacity-0 transition-opacity"
      >
        <source
          src={"desktop-hydra-hero.mp4"}
          type="video/mp4"
          media={forTablet}
          data-testid="video-background-source-wide"
        />
        <source
          src={"mobile-hydra-hero.mp4"}
          type="video/mp4"
          data-testid="video-background-source-narrow"
        />
      </video>
      <div className="pageContainer">
        <div className="component-lg tablet:py-[72px]">
          <div className="pb-9 tablet:max-w-[485px]">
            <motion.h1
              className="tablet:text-[56px] tablet:leading-[67px] text-4xl leading-[48px] text-primary font-medium pb-4"
              initial={{ opacity: 0 }}
              animate={{ opacity: 1 }}
              transition={{ ease: "easeInOut", duration: 0.75, delay: 0.4 }}
            >
              {siteConfig.title}
            </motion.h1>
            <motion.p
              initial={{ opacity: 0 }}
              animate={{ opacity: 1 }}
              transition={{ ease: "easeInOut", duration: 0.75, delay: 1 }}
            >
              The Hydra Head protocol is a layer 2 scaling solution for Cardano
              rooted in peer-reviewed research that increases transaction
              throughput and ensures cost efficiency while maintaining rigorous
              security.
            </motion.p>
          </div>
          <motion.div
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
            transition={{ ease: "easeInOut", duration: 0.75, delay: 1.2 }}
          >
            <Link
              className="inline-block px-4 py-3 border border-solid border-primary font-bold text-sm text-primary rounded-lg no-underline bg-white hover:bg-primary-extralight hover:no-underline hover:text-primary"
              to="/docs/protocol-overview"
            >
              Learn more
            </Link>
          </motion.div>
        </div>
      </div>
    </div>
  );
};

export default HomepageHero;
