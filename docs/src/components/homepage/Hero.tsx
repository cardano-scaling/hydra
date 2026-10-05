import React, { FC } from "react";
import { forTablet } from "../../../helpers/media-queries";
import { motion } from "framer-motion";

const Hero: FC = () => {
  return (
    <section id="homepage-hero">
      <div className="pageContainer">
        <div className="padding-section-bottom-small">
          <h1>Run your app off-chain. Settle it on Cardano.</h1>
        </div>
      </div>
    </section>
  );
};

export default Hero;
