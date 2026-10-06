import { FC, ReactNode, useRef, useState, useEffect } from "react";
import {
  motion,
  useScroll,
  useTransform,
  useSpring,
  useReducedMotion,
} from "framer-motion";
import { RightArrow } from "../icons/Arrow";
import SectionHeading from "./components/SectionHeading";
import { ProtocolFeaturesContent } from "../../../docs/homepage/protocol-features";
import useBaseUrl from "@docusaurus/useBaseUrl";
import useMediaQuery from "../../hooks/useMediaQuery";
import { forLaptop } from "../../../helpers/media-queries";

const ITEM_WIDTH_MOBILE = 280;
const ITEM_WIDTH_LAPTOP = 356;

const GAP_MOBILE = 8;
const GAP_LAPTOP = 32;

const MIN_HEIGHT_MOBILE = 800;
const MIN_HEIGHT_LAPTOP = 800;

const ProtocolFeatures: FC = () => {
  const reduceMotion = useReducedMotion();
  const isLaptop = useMediaQuery(forLaptop);
  const fits = useMediaQuery(
    `(min-height: ${isLaptop ? MIN_HEIGHT_LAPTOP : MIN_HEIGHT_MOBILE}px)`
  );

  // Show static layout if user prefers reduced motion or
  // if the viewport height is too small to accommodate the scrolling effect
  const staticLayout = reduceMotion || !fits;

  return (
    <section
      id="protocol-features"
      className="relative bg-[#090909] text-white border-b border-white/25 overflow-x-clip"
    >
      <div className="pageContainer">
        <div className="padding-section h-auto overflow-visible">
          {staticLayout ? (
            <StaticFeatures />
          ) : (
            <ScrollingFeatures isLaptop={isLaptop} />
          )}
        </div>
      </div>
    </section>
  );
};

export default ProtocolFeatures;

const ScrollingFeatures = ({ isLaptop }: { isLaptop: boolean }) => {
  const containerRef = useRef<HTMLDivElement>(null);
  const [containerWidth, setContainerWidth] = useState(0);

  const { scrollYProgress } = useScroll({
    target: containerRef,
    offset: ["start start", "end end"],
  });

  const { features } = ProtocolFeaturesContent;

  // Measure container width on mount and resize
  useEffect(() => {
    const container = containerRef.current;
    if (!container) return;

    const resizeObserver = new ResizeObserver(() => {
      setContainerWidth(container.offsetWidth);
    });
    resizeObserver.observe(container);

    return () => resizeObserver.disconnect();
  }, []);

  // Calculate total horizontal distance based on track width vs container width
  const itemWidth = isLaptop ? ITEM_WIDTH_LAPTOP : ITEM_WIDTH_MOBILE;
  const itemsGap = isLaptop ? GAP_LAPTOP : GAP_MOBILE;
  const trackWidth =
    features.length * itemWidth + (features.length - 1) * itemsGap;
  const totalDistance = Math.max(0, trackWidth - containerWidth);
  const x = useSpring(
    useTransform(scrollYProgress, [0, 1], [0, -totalDistance]),
    { stiffness: 300, damping: 40, mass: 0.5 }
  );

  return (
    <div
      ref={containerRef}
      className="relative"
      style={{ height: `calc(100dvh + ${totalDistance}px)` }}
    >
      <div className="sticky-wrapper w-full flex flex-col gap-8 tablet:gap-14">
        <Intro />
        <motion.div
          className="flex"
          style={{ x, gap: isLaptop ? GAP_LAPTOP : GAP_MOBILE }}
        >
          {features.map((feature, i) => (
            <Feature
              key={i}
              icon={feature.icon}
              title={feature.title}
              text={feature.text}
              isLaptop={isLaptop}
            />
          ))}
        </motion.div>
        <Cta />
      </div>
    </div>
  );
};

const Intro = () => {
  const { label, title, text } = ProtocolFeaturesContent;
  return (
    <div className="flex flex-col gap-2">
      <SectionHeading label={label} title={title} />
      <p className="text-white/50">{text}</p>
    </div>
  );
};

const Cta = () => {
  const { cta } = ProtocolFeaturesContent;
  const ctaUrl = useBaseUrl(cta.url);
  return (
    <div>
      <a
        href={ctaUrl}
        className="protocol-features__cta inline-flex gap-2 items-center"
      >
        {cta.label} <RightArrow className="size-4" />
      </a>
    </div>
  );
};

const StaticFeatures = () => {
  const { features } = ProtocolFeaturesContent;
  return (
    <div className="relative">
      <div className="w-full flex flex-col gap-8 tablet:gap-14">
        <Intro />
        <div className="grid grid-cols-1 tablet:grid-cols-2 desktop:grid-cols-3 gap-3 desktop:gap-8">
          {features.map((feature, i) => (
            <div key={i}>
              <Feature
                icon={feature.icon}
                title={feature.title}
                text={feature.text}
                staticLayout={true}
              />
            </div>
          ))}
        </div>
        <Cta />
      </div>
    </div>
  );
};

type FeatureProps = {
  icon: ReactNode;
  title: string;
  text: string;
  staticLayout?: boolean;
  isLaptop?: boolean;
};

const Feature = ({
  icon,
  title,
  text,
  staticLayout,
  isLaptop,
}: FeatureProps) => {
  return (
    <div
      className="shrink-0 flex flex-col gap-4 justify-between bg-[var(--hydra-surface)] rounded-3xl p-6 laptop:p-8 min-h-[21.25rem] laptop:min-h-[25rem]"
      style={
        staticLayout
          ? { width: "100%" }
          : isLaptop
          ? { width: ITEM_WIDTH_LAPTOP }
          : { width: ITEM_WIDTH_MOBILE }
      }
    >
      <div>{icon}</div>
      <div className="flex flex-col gap-2">
        <h3 className="protocol-feature__title">{title}</h3>
        <p className="text-white/50">{text}</p>
      </div>
    </div>
  );
};
