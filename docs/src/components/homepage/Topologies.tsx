import { FC } from "react";
import Link from "@docusaurus/Link";
import useBaseUrl from "@docusaurus/useBaseUrl";
import SectionHeading from "./components/SectionHeading";
import LargeLink from "./components/LargeLink";
import { DownUpArrows, RightArrow } from "../icons/Arrow";
import CheckBadge from "../icons/CheckBadge";
import { TopologiesContent } from "../../../docs/homepage/topologies";

type Topology = typeof TopologiesContent.topologies[number];

const Topologies: FC = () => {
  const { label, title, description, topologies, link } = TopologiesContent;
  return (
    <section id="topologies" className="topologies">
      <div className="pageContainer">
        <div className="topologies__inner">
          <SectionHeading
            label={label}
            title={title}
            description={description}
            mode="light"
          />
          <div className="topologies__cards">
            {topologies.map((topology) => (
              <TopologyCard key={topology.label} topology={topology} />
            ))}
          </div>
          <LargeLink to={link.url} mode="light">
            {link.label}
          </LargeLink>
        </div>
      </div>
    </section>
  );
};

export default Topologies;

const TopologyCard: FC<{ topology: Topology }> = ({ topology }) => {
  const imageUrl = useBaseUrl(topology.image);
  const blocksUrl = useBaseUrl("/img/topologies-blocks.svg");
  return (
    <article className="topologies__card">
      <div className="topologies__text">
        <span className="section-label">{topology.label}</span>
        <h3 className="card-title">{topology.title}</h3>
        <p className="section-description topologies__description">
          {topology.description}
        </p>
        <Link
          to={topology.button.url}
          className="link-button link-button-transparent topologies__button"
        >
          <span className="topologies__button-label">
            {topology.button.label}
          </span>
          <span className="topologies__button-label--short">
            {TopologiesContent.shortButtonLabel}
          </span>
          <RightArrow className="size-[1.125rem]" />
        </Link>
      </div>
      <div className="topologies__diagram">
        <div className="topologies__nodes">
          <p className="topologies__signers">
            {topology.signers}
            {topology.allSign && <CheckBadge className="topologies__check" />}
          </p>
          <img
            className="topologies__graphic"
            src={imageUrl}
            alt={topology.imageAlt}
            height={303}
          />
        </div>
        <DownUpArrows className="topologies__arrows" aria-hidden="true" />
        <div className="topologies__settlement">
          <img src={blocksUrl} alt="" width={201} height={50} />
          <span className="section-label">{topology.settlement}</span>
        </div>
      </div>
    </article>
  );
};
