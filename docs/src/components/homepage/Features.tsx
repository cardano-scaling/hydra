import React, { FC } from "react";
import { motion } from "framer-motion";
import { FeatureList } from "../../../docs/homepage/features";
import useMediaQuery from "../../hooks/useMediaQuery";
import { forTablet } from "../../../helpers/media-queries";

type Props = {
  icon: React.JSX.Element;
  title: string;
  description: string;
  index: number;
};

const Feature: FC<Props> = ({ icon, title, description, index }) => {
  const isTabletUp = useMediaQuery(forTablet);
  return (
    <motion.div
      className="flex flex-col gap-4"
      initial="hidden"
      whileInView="visible"
      viewport={{ once: true }}
      transition={{ duration: 0.35, delay: isTabletUp ? index * 0.2 : 0.25 }}
      variants={{
        visible: { opacity: 1, y: 0 },
        hidden: { opacity: 0, y: 100 },
      }}
    >
      <div className="inline-flex gap-3 border-b pb-4 border-gray [&>*:first-child]:mt-1">
        {icon}
        <span className="text-2xl">{title}</span>
      </div>
      <p>{description}</p>
    </motion.div>
  );
};

const Features: FC = () => {
  return (
    <section className="component bg-white">
      <div className="pageContainer">
        <motion.h2
          className="text-base text-primary pb-14 section-label"
          initial="hidden"
          whileInView="visible"
          viewport={{ once: true }}
          transition={{ duration: 0.35, delay: 0.75 }}
          variants={{
            visible: { opacity: 1, y: 0 },
            hidden: { opacity: 0, y: 100 },
          }}
        >
          / FEATURES
        </motion.h2>
        <motion.div className="grid laptop:grid-cols-3 laptop:grid-rows-2 laptop:grid-flow-row tablet:grid-rows-3 tablet:grid-flow-col gap-x-6 tablet:gap-y-6 laptop:gap-y-14 gap-y-14">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} index={idx} {...props} />
          ))}
        </motion.div>
      </div>
    </section>
  );
};

export default Features;
