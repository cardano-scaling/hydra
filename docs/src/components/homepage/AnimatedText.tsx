import React, { FC, useState } from "react";
import { motion } from "framer-motion";
import clsx from "clsx";

const processText = (children: React.ReactNode): React.ReactNode =>
  React.Children.map(children, (child) => {
    if (typeof child === "string") {
      return child.split(" ").map((word, index) => (
        <span key={index} className="inline-block">
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
  const [popTextClass, setPopTextClass] = useState("");
  return (
    <section className="component max-w-[1040px] mx-auto">
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
          <h2 className="homepageText text-primary">
            Provides a{" "}
            <span
              className={clsx("transition-colors duration-500", popTextClass)}
            >
              scalable,
            </span>{" "}
            <span
              className={clsx("transition-colors duration-500", popTextClass)}
            >
              secure,
            </span>
            and{" "}
            <span
              className={clsx("transition-colors duration-500", popTextClass)}
            >
              sustainable
            </span>{" "}
            platform for Cardano, meeting the growing demands of its
            applications and users.
          </h2>
        )}
      </motion.div>
    </section>
  );
};

export default AnimatedText;
