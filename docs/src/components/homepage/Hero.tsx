import { FC } from "react";
import { Badge } from "./components/Badge";
import Code from "../icons/Code";
import { RightArrow } from "../icons/Arrow";
import { RiveWrapperContain } from "./components/RiveWrapper";
import useBaseUrl from "@docusaurus/useBaseUrl";
import { HeroContent } from "../../../docs/homepage/hero";

const Hero: FC = () => {
  return (
    <section
      id="homepage-hero"
      className="relative bg-[#090909] text-white border-b border-white/25 overflow-hidden"
    >
      <div className="hero__background" />
      <div className="relative pageContainer">
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
  const ctaOneUrl = useBaseUrl(HeroContent.buttons[0].url);
  const ctaTwoUrl = useBaseUrl(HeroContent.buttons[1].url);
  return (
    <div className="flex flex-col justify-center gap-8 tablet:gap-10 py-[2.8125rem] max-tablet:pb-0">
      <div className="flex flex-col gap-2">
        <Badge mode="dark">{HeroContent.label}</Badge>
        <h1 className="hero__heading">
          {HeroContent.title[0]}{" "}
          <span className="text-[var(--hydra-red)]">
            {HeroContent.title[1]}
          </span>
        </h1>
        <p className="max-w-[27.875rem] text-white/50">
          {HeroContent.paragraphs[0]}
        </p>
      </div>
      <div className="flex flex-col items-start gap-4 tablet:flex-wrap tablet:flex-row">
        <a href={ctaOneUrl} className="link-button link-button-light">
          {HeroContent.buttons[0].label}{" "}
          <RightArrow className="size-[1.125rem]" />
        </a>
        <a href={ctaTwoUrl} className="link-button link-button-transparent">
          {HeroContent.buttons[1].label} <Code className="w-[1.125rem] h-4" />
        </a>
      </div>
    </div>
  );
};

const RiveAnimation = () => {
  const riveSrc = useBaseUrl("/rive/hero-animation-loop.riv");
  return (
    <div className="relative w-full h-full aspect-[430/588] phablet:aspect-square">
      <div className="absolute inset-[-20%] phablet:inset-0">
        <RiveWrapperContain src={riveSrc} />
      </div>
    </div>
  );
};

const Stats = () => {
  return (
    <ul className="flex-wrap gap-8 hidden laptop:flex">
      {HeroContent.stats.map((stat, i) => (
        <li key={i}>
          <Stat heading={stat.title} text={stat.text} />
        </li>
      ))}
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
