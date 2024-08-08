import React, {
  ComponentPropsWithoutRef,
  FC,
  useCallback,
  useEffect,
  useRef,
  useState,
} from "react";
import clsx from "clsx";
import Square from "../../icons/Square";
import Triangle from "../../icons/Triangle";
import Dot from "../../icons/Dot";
import useMediaQuery from "../../../hooks/useMediaQuery";
import { forTablet } from "../../../../helpers/media-queries";
import { HowItWorksCarouselContent } from "../../../../docs/homepage/how-it-works-carousel";
import { Swiper, SwiperSlide } from "swiper/react";

import "swiper/css";

type Props = {
  description: string;
  idx: number;
};

const CarouselEntryDescription: FC<Props> = ({ description, idx }) => {
  return (
    <div className="flex flex-col tablet:max-w-[478px] max-w-80 tablet:pt-6 pt-14 tablet:min-h-64 min-h-[350px] tablet:order-1 order-2 self-center">
      <div className="flex flex-col tablet:gap-4 max-w-md gap-8">
        <p>{description}</p>
        {idx === 0 && (
          <div className="flex gap-4 tablet:flex-row flex-col">
            <span className="inline-flex gap-[5px]">
              <Square className="mt-1" />
              Passengers <span className="font-bold">(Participants)</span>
            </span>
            <span className="inline-flex gap-[5px]">
              <Triangle className="mt-[3px]" />
              Passengers <span className="font-bold">(Participants)</span>
            </span>
          </div>
        )}
      </div>
    </div>
  );
};

type ControlProps = {
  showing: number;
  onClickBullet: (nth: number) => void;
} & ComponentPropsWithoutRef<"div">;

const Controls: FC<ControlProps> = ({ showing, onClickBullet, ...props }) => {
  return (
    <div
      className="inline-flex mt-auto gap-4 pt-14 self-center order-1 tablet:order-2 tablet:pt-3"
      {...props}
    >
      {HowItWorksCarouselContent.map((_, index) => (
        <button key={`dot-${index}`} onClick={() => onClickBullet(index)}>
          <Dot
            className={clsx(
              "self-center",
              index === showing ? "text-teal" : "text-teal-lightest"
            )}
          />
        </button>
      ))}
    </div>
  );
};

const ResponsiveCarousel: FC = () => {
  const [currentSlide, setCurrentSlide] = useState(0);
  const isTabletUp = useMediaQuery(forTablet);
  const sliderRef = useRef<any>(null);

  const updateIndex = useCallback(
    () => setCurrentSlide(sliderRef.current?.swiper.realIndex),
    []
  );

  const handleClickBullet = useCallback((index: number) => {
    if (!sliderRef.current) return;
    sliderRef.current.swiper.slideTo(index);
  }, []);

  useEffect(() => {
    const swiperInstance = sliderRef.current?.swiper;

    if (swiperInstance) {
      swiperInstance.on("slideChange", updateIndex);
    }

    return () => {
      if (swiperInstance) {
        swiperInstance.off("slideChange", updateIndex);
      }
    };
  }, [updateIndex, isTabletUp]);

  return (
    <section className="bg-[#F4F5F5]">
      <div className="component relative pageContainer flex flex-col">
        <h4 className="text-2xl text-teal font-medium pb-14">How it works</h4>
        <div className="flex self-center flex-col justify-center">
          <Swiper
            key={isTabletUp ? "tablet" : "mobile"}
            slidesPerView={"auto"}
            centeredSlides={isTabletUp}
            spaceBetween={10}
            ref={sliderRef}
          >
            {isTabletUp
              ? HowItWorksCarouselContent.map((props, idx) => (
                  <SwiperSlide key={idx} style={{ width: "fit-content" }}>
                    <img src={props.src} />
                  </SwiperSlide>
                ))
              : HowItWorksCarouselContent.map((props, idx) => (
                  <SwiperSlide key={idx} style={{ width: "fit-content" }}>
                    <img src={props.mobileSrc} className="tablet:ml-4 ml-1" />
                  </SwiperSlide>
                ))}
          </Swiper>
          <CarouselEntryDescription
            idx={currentSlide}
            description={HowItWorksCarouselContent[currentSlide]?.description}
          />
          <Controls onClickBullet={handleClickBullet} showing={currentSlide} />
        </div>
      </div>
    </section>
  );
};

export default ResponsiveCarousel;
