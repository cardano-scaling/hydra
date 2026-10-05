import React, { FC } from "react";
import useBaseUrl from "@docusaurus/useBaseUrl";
import SectionHeading from "./SectionHeading";
import { WhyHydraHeadContent } from "../../../docs/homepage/why-hydra-head";

const WhyHydraHead: FC = () => {
  const { label, title, paragraphs, table } = WhyHydraHeadContent;
  const radialUrl = useBaseUrl("/img/why-hydra-radial.svg");
  return (
    <section id="why-hydra" className="why-hydra">
      <div className="pageContainer">
        <div className="padding-section why-hydra__inner">
          <SectionHeading label={label} title={title} />
          <div className="why-hydra__content">
            <div className="why-hydra__intro">
              <img
                className="why-hydra__radial"
                src={radialUrl}
                width={560}
                height={280}
                alt=""
              />
              {paragraphs.map((paragraph, i) => (
                <p key={i}>{paragraph}</p>
              ))}
            </div>
            <div className="why-hydra__comparison">
              <div
                className="why-hydra__table-scroll"
                role="region"
                aria-label={title}
                tabIndex={0}
              >
                <div className="why-hydra__table-box">
                  <div className="why-hydra__table-highlight" aria-hidden />
                  <table className="why-hydra__table">
                    <colgroup>
                      <col className="why-hydra__col-property" />
                      <col />
                      <col className="why-hydra__col-head" />
                    </colgroup>
                    <thead>
                      <tr>
                        {table.columns.map((column) => (
                          <th key={column} scope="col">
                            {column}
                          </th>
                        ))}
                      </tr>
                    </thead>
                    <tbody>
                      {table.rows.map((row) => (
                        <tr key={row.property}>
                          <th scope="row">{row.property}</th>
                          <td>{row.mainchain}</td>
                          <td>{row.head}</td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>
  );
};

export default WhyHydraHead;
