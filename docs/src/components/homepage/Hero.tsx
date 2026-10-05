import { FC } from "react";
import { Badge } from "./Badge";
import Code from "../icons/Code";
import { RightArrow } from "../icons/Arrow";
import { RiveWrapperContain } from "./RiveWrapper";
import useBaseUrl from "@docusaurus/useBaseUrl";

const Hero: FC = () => {
  return (
    <section
      id="homepage-hero"
      className="bg-[var(--pure-black)] text-white border-b border-white/25 overflow-hidden"
    >
      <div className="pageContainer">
        <div className="padding-section-bottom-small flex flex-col gap-2">
          <div className="grid tablet:grid-cols-2">
            <Content />
            <RiveAnimation />
          </div>
          <Stats />
        </div>
      </div>
    </section>
  );
};

export default Hero;

const Content = () => {
  return (
    <div className="flex flex-col justify-center gap-8 tablet:gap-10">
      <div className="flex flex-col gap-2">
        <Badge mode="dark">Hydra 2.3.0 Live on Cardano mainnet</Badge>
        <h1 className="hero__heading">
          Run your app off-chain.{" "}
          <span className="text-[var(--hydra-red)]">Settle it on Cardano.</span>
        </h1>
        <p className="max-w-[27.875rem] text-white/50">
          Hydra runs your app in a head, a fast lane beside Cardano.
          Transactions confirm in under a second, can cost nothing, and settle
          on Cardano.
        </p>
      </div>
      <div className="flex flex-col items-start gap-4 tablet:flex-wrap tablet:flex-row">
        <a href="/docs" className="link-button link-button-light">
          Open your first head <RightArrow className="size-[1.125rem]" />
        </a>
        <a href="/docs" className="link-button link-button-transparent">
          Read the Docs <Code className="w-[1.125rem] h-4" />
        </a>
      </div>
    </div>
  );
};

const RiveAnimation = () => {
  const riveSrc = useBaseUrl("/rive/hero-animation-loop.riv");
  return (
    <div className="relative w-full h-full aspect-[430/588] phablet:aspect-[800/976]">
      <div className="absolute inset-[-20%] phablet:inset-0">
        <RiveWrapperContain src={riveSrc} />
      </div>
    </div>
  );
};

const Stats = () => {
  return (
    <ul className="flex-wrap gap-8 hidden laptop:flex">
      <li>
        <Stat heading="< 1s" text="To confirm a transaction" />
      </li>
      <li>
        <Stat heading="Zero fees" text="Or whatever you set" />
      </li>
      <li>
        <Stat heading="1:1" text="Same smart contracts as Cardano" />
      </li>
    </ul>
  );
};

const Stat = ({ heading, text }: { heading: string; text: string }) => {
  return (
    <div className="flex flex-col gap-2 py-4 px-6">
      <h2 className="text-[3rem] leading-[1.17] tracking-[-2%]">{heading}</h2>
      <p className="text-white/50">{text}</p>
    </div>
  );
};
