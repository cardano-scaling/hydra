import React, { FC, useState } from "react";

import clsx from "clsx";
import Arrow from "../icons/Arrow";
import { forLaptop, forTablet } from "../../../helpers/media-queries";
import useMediaQuery from "../../hooks/useMediaQuery";
import { motion } from "framer-motion";
import { HowItWorksContent } from "../../../docs/homepage/how-it-works";

const HowItWorks: FC = () => {
  const [expanded, setExpanded] = useState(false);
  const isTabletUp = useMediaQuery(forTablet);
  const isLaptopUp = useMediaQuery(forLaptop);
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
      <h4 className="text-base text-teal pb-14">/ HOW IT WORKS</h4>
      <motion.div
        className="laptop:flex laptop:flex-row grid"
        initial={isLaptopUp && "hidden"}
        animate={expanded && isLaptopUp ? "visible" : "hidden"}
        transition={{ duration: 0.15 }}
        variants={
          isLaptopUp
            ? {
                visible: { height: "100%", overflow: "visible" },
                hidden: { height: 320, overflow: "hidden" },
              }
            : {}
        }
      >
        <div className="flex flex-col gap-4 laptop:w-[472px] basis-[32%] laptop:pt-0 pt-4 order-2 laptop:-order-1">
          <h4 className="text-2xl color-darkRed font-medium text-darkRed">
            {HowItWorksContent.title}
          </h4>
          <p>{HowItWorksContent.descriptionParagraphOne}</p>

          <motion.p
            className="z-10"
            initial="hidden"
            animate={expanded ? "visible" : "hidden"}
            transition={{ duration: 0.3, ease: "easeInOut" }}
            variants={{
              visible: { opacity: 1, y: 0, height: "100%" },
              hidden: { opacity: 0, y: -30, height: 0 },
            }}
          >
            {HowItWorksContent.descriptionParagraphTwo}
          </motion.p>
          <motion.p
            className="z-10"
            initial="hidden"
            animate={expanded ? "visible" : "hidden"}
            transition={{ duration: 0.3, ease: "easeInOut" }}
            variants={{
              visible: { opacity: 1, y: 0, height: "100%" },
              hidden: { opacity: 0, y: -50, height: 0 },
            }}
          >
            {HowItWorksContent.descriptionParagraphThree}
          </motion.p>

          <div className="w-full z-20">
            <button
              className="bg-none text-teal self-start inline-flex gap-3 group"
              onClick={() => setExpanded(!expanded)}
            >
              {`Read ${expanded ? "less" : "more"}`}{" "}
              <Arrow
                className={clsx(
                  "mt-1 rounded-full group-hover:bg-teal/15",
                  expanded ? "-rotate-90" : "rotate-90"
                )}
              />
            </button>
          </div>
        </div>
        {isTabletUp && (
          <motion.div
            className="basis-[60%] image border-b border-solid border-teal inline-block -order-1 laptop:order-2 transition-all duration-200 ease-in-out"
            initial="hidden"
            animate={expanded ? "visible" : "hidden"}
            transition={{ duration: 0.15 }}
            variants={{
              visible: { height: "100%", overflow: "visible" },
              hidden: { height: 320, overflow: "hidden" },
            }}
          >
            <img
              src="hydra-docs-landing-graphic.png"
              className="-z-10 overflow-visible"
            />
          </motion.div>
        )}
      </motion.div>
    </motion.section>
  );
};

export default HowItWorks;
