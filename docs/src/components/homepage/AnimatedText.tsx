import React, { FC, useState } from "react";
import useMediaQuery from "../../hooks/useMediaQuery";
import { forLaptop } from "../../../helpers/media-queries";
import { motion } from "framer-motion";
import clsx from "clsx";

const processText = (children: React.ReactNode): React.ReactNode =>
  React.Children.map(children, (child) => {
    if (typeof child === "string") {
      return child.split(" ").map((word, index) => (
        <span key={index} className="whitespace-nowrap">
          {word.split("").map((char, charIndex) => (
            <motion.span
              key={charIndex}
              className="inline-flex"
              variants={{
                hidden: { opacity: 0, y: -10, rotate: -2 },
                visible: {
                  opacity: 1,
                  y: 0,
                  rotate: 0,
                  transition: {
                    duration: 0.12,
                    ease: "easeOut",
                  },
                },
              }}
            >
              {char}
              {charIndex === word.length - 1 && (
                <span className="inline-flex">&nbsp;</span>
              )}
            </motion.span>
          ))}
        </span>
      ));
    }

    if (React.isValidElement(child)) {
      const processedText = processText(child.props.children);
      return React.cloneElement(child, child.props, processedText);
    }
  });

const AnimatedText: FC = () => {
  const isLaptopUp = useMediaQuery(forLaptop);
  const [popTextClass, setPopTextClass] = useState("");
  return (
    <section className="component tablet:px-[72px]">
      {isLaptopUp ? (
        <motion.div
          className="homepageText"
          variants={{
            hidden: {},
            visible: {
              transition: {
                staggerChildren: 0.025,
                delayChildren: 0,
              },
            },
          }}
          initial="hidden"
          whileInView="visible"
          viewport={{
            once: true,
          }}
          onAnimationComplete={() => setPopTextClass("text-[#696E70]")}
        >
          {processText(
            <>
              <h2 className="homepageText">
                Provides a{" "}
                <span
                  className={clsx(
                    "transition-colors duration-500",
                    popTextClass
                  )}
                >
                  scalable
                </span>
                ,{" "}
                <span
                  className={clsx(
                    "transition-colors duration-500",
                    popTextClass
                  )}
                >
                  secure
                </span>
                , and{" "}
                <span
                  className={clsx(
                    "transition-colors duration-500",
                    popTextClass
                  )}
                >
                  sustainable
                </span>{" "}
                platform
              </h2>
              <h2 className="homepageText">
                for Cardano, meeting the growing demands of its
              </h2>
              <h2 className="homepageText">applications and users.</h2>
            </>
          )}
        </motion.div>
      ) : (
        <motion.div
          className="homepageText"
          variants={{
            hidden: {},
            visible: {
              transition: {
                staggerChildren: 0.025,
                delayChildren: 0,
              },
            },
          }}
          initial="hidden"
          whileInView="visible"
          viewport={{
            once: true,
          }}
          onAnimationComplete={() => setPopTextClass("text-[#696E70]")}
        >
          {processText(
            <h2 className="tablet:homepageText tablet:text-center tablet:text-10 text-left text-[32px] text-teal">
              Provides a <span className="text-[#696E70]">scalable</span>,{" "}
              <span className="text-[#696E70]">secure</span>, and{" "}
              <span className="text-[#696E70]">sustainable</span> platform for
              Cardano, meeting the growing demands of its applications and
              users.
            </h2>
          )}
        </motion.div>
      )}
    </section>
  );
};

export default AnimatedText;
