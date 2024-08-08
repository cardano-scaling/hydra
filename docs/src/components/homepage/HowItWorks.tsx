import React, { FC, useState } from "react";
import clsx from "clsx";
import Arrow from "../icons/Arrow";
import { forLaptop, forTablet } from "../../../helpers/media-queries";
import useMediaQuery from "../../hooks/useMediaQuery";
import { motion } from "framer-motion";
import { HowItWorksContent } from "../../../docs/homepage/how-it-works";
import { useWindowSize } from "../../hooks/useWindowSize";

const HowItWorks: FC = () => {
  const windowSize = useWindowSize(300);
  const [expanded, setExpanded] = useState(false);
  const isTabletUp = useMediaQuery(forTablet);
  const isLaptopUp = useMediaQuery(forLaptop);

  // Reset inline height set by framer motion
  const key = `${windowSize.width}-${windowSize.height}`;

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
      <h5 className="text-base text-teal pb-14">/ HOW IT WORKS</h5>
      <motion.div
        key={key}
        className="grid laptop:flex laptop:flex-row laptop:gap-6"
        initial={"hidden"}
        animate={expanded ? "visible" : "hidden"}
        transition={{ duration: 0.15 }}
        variants={{
          visible: { height: "auto", overflow: "visible" },
          hidden: { overflow: "hidden" },
        }}
      >
        <div className="flex flex-col basis-[41%] pt-4 order-2 laptop:-order-1 laptop:w-[472px] laptop:pt-0">
          <h4 className="text-2xl color-darkRed font-medium text-darkRed pb-4">
            {HowItWorksContent.title}
          </h4>
          <p>{HowItWorksContent.descriptionParagraphOne}</p>

          <motion.div
            initial="hidden"
            animate={expanded ? "visible" : "hidden"}
            transition={{ duration: 0.1, ease: "easeInOut" }}
            variants={{
              visible: { opacity: 1, height: "auto" },
              hidden: { opacity: 0, height: 0 },
            }}
          >
            <p className="py-4">{HowItWorksContent.descriptionParagraphTwo}</p>
            <p>{HowItWorksContent.descriptionParagraphThree}</p>
          </motion.div>
          <div className="w-full relative mt-4">
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
            className="basis-[59%] relative border-b border-solid border-teal -order-1 laptop:order-2"
            initial="hidden"
            animate={expanded ? "visible" : "hidden"}
            transition={{ duration: 0.15 }}
            variants={{
              visible: isLaptopUp
                ? { overflow: "visible" }
                : { overflow: "visible", height: "auto" },
              hidden: isLaptopUp
                ? { overflow: "hidden" }
                : { overflow: "hidden", height: 280 },
            }}
          >
            <img
              src="hydra-docs-landing-graphic.png"
              className="w-full laptop:absolute"
            />
          </motion.div>
        )}
      </motion.div>
    </motion.section>
  );
};

export default HowItWorks;
