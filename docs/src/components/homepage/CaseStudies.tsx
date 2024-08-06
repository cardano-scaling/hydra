import React, { FC } from "react";
import Link from "@docusaurus/Link";
import Arrow from "../icons/Arrow";
import useMediaQuery from "../../hooks/useMediaQuery";
import { forLaptop } from "../../../helpers/media-queries";
import { motion } from "framer-motion";
import { FeaturedCaseStudy } from "../../../docs/homepage/case-studies";

const CaseStudies: FC = () => {
  const isLaptopUp = useMediaQuery(forLaptop);
  return (
    <section className="bg-[#E7EEF0]">
      <motion.div
        className="component pageContainer"
        initial="hidden"
        whileInView="visible"
        viewport={{ once: true }}
        transition={{ duration: 0.35, delay: 0.25 }}
        variants={{
          visible: { opacity: 1, y: 0 },
          hidden: { opacity: 0, y: 100 },
        }}
      >
        <h5 className="text-base text-teal pb-14">/ CASE STUDIES</h5>
        <div className="bg-white flex tablet:flex-row flex-col rounded-2xl laptop:mb-10 tablet:p-8 justify-between gap-8">
          <div className="flex flex-col laptop:pt-20 gap-8 laptop:max-w-md px-4 py-8 tablet:p-0 tablet:basis-2/5">
            <h4 className="text-2xl text-teal font-medium">Case Studies</h4>
            <p>{FeaturedCaseStudy.content}</p>
            <Link
              className="bg-none text-teal self-start inline-flex gap-3 group hover:no-underline hover:text-teal"
              href="/"
            >
              {"View case studies "}
              <Arrow className="mt-1 rounded-full group-hover:bg-teal/15" />
            </Link>
          </div>
          <motion.div
            className="tablet:basis-3/5 self-center w-full flex justify-end"
            initial="hidden"
            whileInView="visible"
            viewport={{ once: true }}
            transition={{ duration: 0.5, delay: 0.8, ease: "easeInOut" }}
            variants={{
              visible: { opacity: 1 },
              hidden: { opacity: 0 },
            }}
          >
            <img
              width={!isLaptopUp ? "100%" : "auto"}
              src={
                isLaptopUp ? FeaturedCaseStudy.src : FeaturedCaseStudy.mobileSrc
              }
              className="tablet:rounded-2xl rounded-none"
            />
          </motion.div>
        </div>
      </motion.div>
    </section>
  );
};

export default CaseStudies;
