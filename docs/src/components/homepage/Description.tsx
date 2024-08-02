import React, { FC } from "react";
import { translate } from "@docusaurus/Translate";
import useMediaQuery from "../../hooks/useMediaQuery";
import { forLaptop } from "../../../helpers/media-queries";

const Description: FC = () => {
  const isLaptopUp = useMediaQuery(forLaptop);
  return (
    <section className="component tablet:px-[72px]">
      {isLaptopUp ? (
        <>
          <h2 className="homepageText">
            Provides a <span className="text-[#696E70]">scalable</span>,{" "}
            <span className="text-[#696E70]">secure</span>, and{" "}
            <span className="text-[#696E70]">sustainable</span> platform
          </h2>
          <h2 className="homepageText">
            for Cardano, meeting the growing demands of its
          </h2>
          <h2 className="homepageText">applications and users.</h2>
        </>
      ) : (
        <h2 className="tablet:homepageText text-left text-[32px] text-teal">
          Provides a <span className="text-[#696E70]">scalable</span>,{" "}
          <span className="text-[#696E70]">secure</span>, and{" "}
          <span className="text-[#696E70]">sustainable</span> platform for
          Cardano, meeting the growing demands of its applications and users.
        </h2>
      )}
    </section>
  );
};

export default Description;
