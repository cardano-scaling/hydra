import React, { FC } from "react";
import Link from "@docusaurus/Link";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import Translate from "@docusaurus/Translate";
import { forTablet } from "../../../helpers/media-queries";

const HomepageHero: FC = () => {
  const { siteConfig } = useDocusaurusContext();
  return (
    <div>
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
        <div className="component my-6">
          <div className="pb-8 tablet:max-w-md max-w-72">
            <h1 className="tablet:text-[56px] text-4xl leading-[48px] text-teal font-medium pb-4">
              {siteConfig.title}
            </h1>
            <p>
              <Translate
                id="homepage.tagline"
                description="Homepage tagline, below title."
              >
                Hydra Head protocol is a layer 2 scaling solution for Cardano
                rooted in peer-reviewed research that increases transaction
                throughput and ensures cost efficiency while maintaining
                rigorous security.
              </Translate>
            </p>
          </div>
          <Link
            className="px-4 py-3 justify-center border border-solid border-teal font-bold text-teal rounded-lg no-underline hover:bg-teal/15 hover:no-underline hover:text-teal
      "
            to="/docs/getting-started"
          >
            Learn More
          </Link>
        </div>
      </div>
    </div>
  );
};

export default HomepageHero;
