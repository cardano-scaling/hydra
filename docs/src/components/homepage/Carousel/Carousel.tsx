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
    <div className="flex items-center gap-6 h-full justify-center">
      <div className="basis-[40%]">
        <img
          src={src}
          className="w-full rounded-lg object-cover"
          alt={`How it works panel ${idx}`}
        />
      </div>
      <div className="flex flex-col gap-4 max-w-md justify-center">
        <h4 className="text-2xl text-primary font-medium">How it works</h4>
        <p>{description}</p>
        {idx === 0 && (
          <div className="flex gap-4">
            <span className="inline-flex gap-[5px]">
              <Square className="mt-1" />
              Passengers <span className="font-bold">(participants)</span>
            </span>
            <span className="inline-flex gap-[5px]">
              <Triangle className="mt-[3px]" />
              Supplies <span className="font-bold">(tokens)</span>
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
      <button
        onClick={handlePrev}
        disabled={showing < 1}
        aria-label="Previous slide"
      >
        <Arrow
          className={clsx(
            "rotate-180 rounded-full",
            showing < 1
              ? "text-primary-lightest"
              : "text-primary hover:bg-primary/15"
          )}
        />
      </button>
      {HowItWorksCarouselContent.map((_, index) => (
        <Dot
          key={`dot-${index}`}
          className={clsx(
            "self-center",
            index === showing ? "text-primary" : "text-primary-lightest"
          )}
        />
      ))}
      <button
        onClick={handleNext}
        disabled={showing > HowItWorksCarouselContent.length - 2}
        aria-label="Next slide"
      >
        <Arrow
          className={clsx(
            "rounded-full",
            showing > HowItWorksCarouselContent.length - 2
              ? "text-primary-lightest"
              : "text-primary hover:bg-primary/15"
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
            <SwiperSlide key={idx}>
              <CarouselEntry idx={idx} {...props} />
            </SwiperSlide>
          ))}
          <div className="flex justify-center w-full gap-6 pt-8">
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
