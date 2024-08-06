import React, { FC, useCallback, useEffect, useRef, useState } from "react";
import clsx from "clsx";
import Square from "../../icons/Square";
import Triangle from "../../icons/Triangle";
import Arrow from "../../icons/Arrow";
import Dot from "../../icons/Dot";
import { Pagination } from "swiper/modules";

import { Swiper, SwiperSlide } from "swiper/react";

import "swiper/css";
import { HowItWorksCarouselContent } from "../../../../docs/homepage/how-it-works-carousel";

type Props = {
  idx: number;
  src: string;
  description: string;
};

const CarouselEntry: FC<Props> = ({ idx, src, description }) => {
  return (
    <div className="flex gap-6 h-full">
      <div className="basis-[45%]">
        <img
          src={src}
          className="w-full h-full max-h-[294px]
        "
        />
      </div>
      <div
        className={clsx(
          "flex flex-col gap-4 max-w-md",
          idx !== 3 ? "pt-11" : "pt-0"
        )}
      >
        <h4 className="text-2xl text-teal font-medium">How it works</h4>
        <p>{description}</p>
        {idx === 0 && (
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
        )}
      </div>
    </div>
  );
};

type ControlProps = {
  showing: number;
  handlePrev: () => void;
  handleNext: () => void;
};

const Controls: FC<ControlProps> = ({ showing, handlePrev, handleNext }) => {
  return (
    <div className="inline-flex gap-4">
      <button onClick={handlePrev} disabled={showing < 1}>
        <Arrow
          className={clsx(
            "rotate-180 rounded-full",
            showing < 1 ? "text-teal-lightest" : "text-teal hover:bg-teal/15"
          )}
        />
      </button>
      {HowItWorksCarouselContent.map((_, index) => (
        <Dot
          className={clsx(
            "self-center",
            index === showing ? "text-teal" : "text-teal-lightest"
          )}
        />
      ))}
      <button
        onClick={handleNext}
        disabled={showing > HowItWorksCarouselContent.length - 2}
      >
        <Arrow
          className={clsx(
            "rounded-full",
            showing > HowItWorksCarouselContent.length - 2
              ? "text-teal-lightest"
              : "text-teal hover:bg-teal/15"
          )}
        />
      </button>
    </div>
  );
};

const Carousel: FC = () => {
  const [currentSlide, setCurrentSlide] = useState(0);
  const sliderRef = useRef<any>(null);
  const handlePrev = useCallback(() => {
    if (!sliderRef.current) return;
    sliderRef.current.swiper.slidePrev();
  }, []);

  const handleNext = useCallback(() => {
    if (!sliderRef.current) return;
    sliderRef.current.swiper.slideNext();
  }, []);

  const updateIndex = useCallback(
    () => setCurrentSlide(sliderRef.current?.swiper.realIndex),
    []
  );

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
  }, [updateIndex]);

  return (
    <section className="bg-[#F4F5F5]">
      <div className="component pageContainer">
        <Swiper
          modules={[Pagination]}
          spaceBetween={50}
          slidesPerView={1}
          ref={sliderRef}
        >
          {HowItWorksCarouselContent.map((props, idx) => (
            <SwiperSlide>
              <CarouselEntry key={idx} idx={idx} {...props} />
            </SwiperSlide>
          ))}
          <div className="flex w-full gap-6 pt-4">
            <span className="basis-[45%]" />
            <Controls
              showing={currentSlide}
              handlePrev={handlePrev}
              handleNext={handleNext}
            />
          </div>
        </Swiper>
      </div>
    </section>
  );
};

export default Carousel;
