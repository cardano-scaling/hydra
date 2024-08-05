import React, { FC, useState } from "react";
import { translate } from "@docusaurus/Translate";
import clsx from "clsx";
import Square from "../../icons/Square";
import Triangle from "../../icons/Triangle";
import Arrow from "../../icons/Arrow";
import Dot from "../../icons/Dot";
import { motion } from "framer-motion";

export const CarouselContent = [
  {
    description: translate({
      id: "homepage.carousel.panelOneText",
      message:
        "Think of a Hydra head as an airplane traveling between two points. Before departure, passengers (participants) and supplies (tokens) are loaded on board. ",
    }),
    src: require("@site/static/img/how-it-works-panel-one.png").default,
    mobileSrc: require("@site/static/img/how-it-works-panel-one-mobile.png")
      .default,
  },
  {
    description: translate({
      id: "homepage.carousel.panelTwoText",
      message:
        "Once in flight, no new passengers or supplies can be added or removed, but the supplies can be rearranged within the plane. This internal movement can include executing agreements (smart contracts), determining which items go where, and ensuring proper validations.",
    }),
    src: require("@site/static/img/how-it-works-panel-two.png").default,
    mobileSrc: require("@site/static/img/how-it-works-panel-two-mobile.png")
      .default,
  },
  {
    description: translate({
      id: "homepage.carousel.panelThreeText",
      message:
        "While the plane is in motion, the direct courier (blockchain) continues to operate independently.",
    }),
    src: require("@site/static/img/how-it-works-panel-three.png").default,
    mobileSrc: require("@site/static/img/how-it-works-panel-three-mobile.png")
      .default,
  },
  {
    description: translate({
      id: "homepage.carousel.panelFourText",
      message:
        "When the plane reaches its destination (head closure), only the final arrangement of supplies is reported back to the courier. It doesn’t matter if you shift things around once or 10 million times, only the end result is communicated to the courier. There is no limit to how many planes can be in the air at any given time; if you need more capacity, you can send up more. Thus, the Hydra head acts as a parallel processing unit, enhancing scalability and efficiency without burdening the main network.",
    }),
    src: require("@site/static/img/how-it-works-panel-four.png").default,
    mobileSrc: require("@site/static/img/how-it-works-panel-four-mobile.png")
      .default,
  },
];

type Props = {
  src: string;
  description: string;
};

const CarouselEntry: FC<Props> = ({ src, description }) => {
  return (
    <div className="flex gap-6 h-full min-h-[340px]">
      <div className="basis-5/12">
        <img src={src} width={600} />
      </div>
      <div className="flex flex-col gap-4 max-w-md basis-7/12">
        <h4 className="text-2xl text-teal font-medium">How it works</h4>
        <p>{description}</p>
        <div className="flex gap-4">
          <span className="inline-flex gap-[5px]">
            <Square className="mt-1" />
            Passengers <span className="font-bold">(Participants)</span>
          </span>
          <span className="inline-flex">
            <Triangle className="mt-[3px] gap-[5px]" />
            Passengers <span className="font-bold">(Participants)</span>
          </span>
        </div>
      </div>
    </div>
  );
};

type ControlProps = {
  showing: number;
  changeShowing: React.Dispatch<React.SetStateAction<number>>;
};

const Controls: FC<ControlProps> = ({ showing, changeShowing }) => {
  return (
    <div className="inline-flex gap-4 z-50 self-center">
      <button onClick={() => changeShowing(showing - 1)} disabled={showing < 1}>
        <Arrow
          className={clsx(
            "rotate-180 rounded-full",
            showing < 1 ? "text-teal-lightest" : "text-teal hover:bg-teal/15"
          )}
        />
      </button>
      {CarouselContent.map((_, index) => (
        <Dot
          className={clsx(
            "self-center",
            index === showing ? "text-teal" : "text-teal-lightest"
          )}
        />
      ))}
      <button
        onClick={() => changeShowing(showing + 1)}
        disabled={showing > CarouselContent.length - 2}
      >
        <Arrow
          className={clsx(
            "rounded-full",
            showing > CarouselContent.length - 2
              ? "text-teal-lightest"
              : "text-teal hover:bg-teal/15"
          )}
        />
      </button>
    </div>
  );
};

const Carousel: FC = () => {
  const [showing, setShowing] = useState(0);
  return (
    <section className="bg-[#F4F5F5]">
      <div className="component relative pageContainer flex flex-col">
        <div className="grid">
          {CarouselContent.map((props, idx) => (
            <motion.div
              className="col-start-1 row-start-1"
              animate={idx === showing ? "visible" : "hidden"}
              transition={{ duration: 0.3, ease: "easeInOut" }}
              variants={{
                visible: { opacity: 1, x: 0 },
                hidden: { opacity: 0, x: -1000 },
              }}
            >
              <CarouselEntry key={idx} {...props}></CarouselEntry>
            </motion.div>
          ))}
        </div>
        <Controls showing={showing} changeShowing={setShowing} />
      </div>
    </section>
  );
};

export default Carousel;
