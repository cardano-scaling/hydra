import React, { FC } from "react";
import { translate } from "@docusaurus/Translate";
import { forLaptop } from "../../../helpers/media-queries";
import useMediaQuery from "../../hooks/useMediaQuery";

const FeaturesContent = [
  {
    title: translate({
      id: "homepage.featureFAQ.openSource.title",
      message: "Open source",
    }),
    description: translate({
      id: "homepage.featureFAQ.openSource.description",
      message:
        "The Cardano developer community has full access to the source code for greater collaboration and innovation. Numerous developers are actively engaged in its continuous improvement and iteration.",
    }),
  },
  {
    title: translate({
      id: "homepage.featureFAQ.interoperable.title",
      message: "Interoperable",
    }),
    description: translate({
      id: "homepage.featureFAQ.interoperable.description",
      message:
        "Designed for seamless integration with existing and future Cardano infrastructure, promoting a cohesive ecosystem and allowing for easy integration with existing DApps and wallets.",
    }),
  },
  {
    title: translate({
      id: "homepage.featureFAQ.isomorphic.title",
      message: "Isomorphic",
    }),
    description: translate({
      id: "homepage.featureFAQ.isomorphic.description",
      message:
        "Same transaction format and ledger rules on layer 2 as on the Cardano layer 1. This includes all the features you know and love from Cardano.",
    }),
  },
  {
    title: translate({
      id: "homepage.featureFAQ.customizable.title",
      message: "Customizable",
    }),
    description: translate({
      id: "homepage.featureFAQ.customizable.description",
      message:
        "Adjustable memory, CPU execution budgets, and other parameters on a per-head basis, allowing for flexible applications – even some that would not be possible on layer 1!",
    }),
  },
  {
    title: translate({
      id: "homepage.featureFAQ.efficient.title",
      message: "Efficient",
    }),
    description: translate({
      id: "homepage.featureFAQ.efficient.description",
      message:
        "Facilitates multi-party agreements, cost-effective micropayments, and offers near-instant transaction finality.",
    }),
  },
];

type Props = {
  title: string;
  description: string;
};

const FAQ: FC<Props> = ({ title, description }) => {
  return (
    <div className="flex text-white tablet:flex-row flex-col">
      <span className=" text-2xl min-w-40 tablet:self-center">{title}</span>
      <p className="tablet:border-l border-t border-solid border-white/25 tablet:pl-12 tablet:ml-12 mt-3 pt-3 max-w-xl">
        {description}
      </p>
    </div>
  );
};

const FeaturesFAQ: FC = () => {
  const isLaptopUp = useMediaQuery(forLaptop);
  return (
    <section className="bg-teal">
      <div className="component pageContainer flex flex-col">
        <h4 className="text-base text-white">/ FEATURES</h4>
        <div className="pt-14 laptop:pr-14 flex flex-col gap-8">
          {FeaturesContent.map((props, idx) => (
            <FAQ key={idx} {...props} />
          ))}
        </div>
        {isLaptopUp && (
          <div className="w-[345px] h-[405px]">
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
              className="rounded-full object-none"
            >
              {/* <source src={"features-video.mov"} type="video/mp4" /> */}
            </video>
          </div>
        )}
      </div>
    </section>
  );
};

export default FeaturesFAQ;
