import React, { FC } from "react";
import clsx from "clsx";
import { motion } from "framer-motion";
import { FeatureList } from "../../../docs/homepage/features";

type Props = {
  icon: React.JSX.Element;
  title: string;
  description: string;
  tagLine: string;
};

const Feature: FC<Props> = ({ icon, title, description, tagLine }) => {
  return (
    <motion.div
      className="flex flex-col gap-4"
      initial="hidden"
      whileInView="visible"
      viewport={{ once: true }}
      transition={{ duration: 0.35, delay: 0.25 }}
      variants={{
        visible: { opacity: 1, y: 0 },
        hidden: { opacity: 0, y: 100 },
      }}
    >
      <div className="inline-flex gap-3 border-b pb-4 border-gray [&>*:first-child]:mt-1">
        {icon}
        <h6 className="text-2xl">{title}</h6>
      </div>
      <p>{description}</p>
      <span
        className={clsx(
          "p-2 w-fit text-sm",
          tagLine === "Fast and cheap transactions"
            ? "text-teal bg-[#F3F6F8]"
            : "text-darkRed bg-[#F8F4F6]"
        )}
      >
        {tagLine}
      </span>
    </motion.div>
  );
};

const Features: FC = () => {
  return (
    <section className="component bg-white">
      <div className="pageContainer">
        <motion.h5
          className="text-base text-teal pb-[72px]"
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
        </motion.h5>
        <motion.div className="grid laptop:grid-cols-3 laptop:grid-rows-2 laptop:grid-flow-row tablet:grid-rows-3 tablet:grid-flow-col gap-x-6 tablet:gap-y-6 laptop:gap-y-14 gap-y-14">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </motion.div>
      </div>
    </section>
  );
};

export default Features;
