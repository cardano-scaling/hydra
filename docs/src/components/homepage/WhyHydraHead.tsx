import React, { FC, useState } from "react";
import clsx from "clsx";
import Arrow from "../icons/Arrow";
import { motion } from "framer-motion";
import { WhyHydraHeadContent } from "../../../docs/homepage/why-hydra-head";

const WhyHydraHead: FC = () => {
  const [expanded, setExpanded] = useState(false);

  return (
    <motion.section
      className="component"
      initial="hidden"
      whileInView="visible"
      viewport={{ once: true }}
      transition={{ duration: 0.35, delay: 0.25 }}
      variants={{
        visible: { opacity: 1, y: 0 },
        hidden: { opacity: 0, y: 100 },
      }}
    >
      <div className="grid laptop:flex laptop:flex-row laptop:gap-6">
        <div className="flex flex-col basis-[41%] pt-4 order-2 laptop:-order-1 laptop:w-[472px] laptop:pt-0">
          <h3 className="text-2xl text-primary font-medium pb-4">
            {WhyHydraHeadContent.title}
          </h3>
          <p>{WhyHydraHeadContent.descriptionParagraphOne}</p>

          <motion.div
            initial="hidden"
            animate={expanded ? "visible" : "hidden"}
            transition={{ duration: 0.1, ease: "easeInOut" }}
            variants={{
              visible: { opacity: 1, height: "auto" },
              hidden: { opacity: 0, height: 0 },
            }}
          >
            <p className="py-4">
              {WhyHydraHeadContent.descriptionParagraphTwo}
            </p>
            <p>{WhyHydraHeadContent.descriptionParagraphThree}</p>
          </motion.div>
          <div className="w-full relative mt-4">
            <button
              className="bg-none text-primary self-start inline-flex gap-3 group"
              onClick={() => setExpanded(!expanded)}
            >
              {`Read ${expanded ? "less" : "more"}`}{" "}
              <Arrow
                className={clsx(
                  "mt-[3px] rounded-full group-hover:bg-primary/15",
                  expanded ? "-rotate-90" : "rotate-90"
                )}
              />
            </button>
          </div>
        </div>
        <div className="hidden basis-[59%] -order-1 laptop:order-2 tablet:block">
          <img
            src="hydra-docs-landing-graphic.png"
            alt="Hydra Head blockchain flowchart"
            className="border-b border-solid border-primary laptop:border-none"
          />
        </div>
      </div>
    </motion.section>
  );
};

export default WhyHydraHead;
