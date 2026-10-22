import useBaseUrl from "@docusaurus/useBaseUrl";
import React, {
  ComponentPropsWithoutRef,
  FC,
  useCallback,
  useEffect,
  useRef,
  useState,
} from "react";
import clsx from "clsx";
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

const CarouselEntryDescription: FC<Props> = ({ description }) => {
  return (
    <div className="flex flex-col tablet:max-w-[478px] max-w-80 tablet:pt-6 pt-14 tablet:order-1 order-2 self-center">
      <div className="flex flex-col tablet:gap-4 max-w-md gap-8">
        <p>{description}</p>
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
      className="inline-flex mt-auto gap-4 pt-14 self-center order-1 tablet:order-2"
      {...props}
    >
      {HowItWorksCarouselContent.map((_, index) => (
        <button key={`dot-${index}`} onClick={() => onClickBullet(index)}>
          <Dot
            className={clsx(
              "self-center",
              index === showing ? "text-primary" : "text-primary-lightest"
            )}
          />
        </button>
      ))}
    </div>
  );
};

const ResponsiveCarousel: FC = () => {
  const baseUrl = useBaseUrl("/");
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
        <h4 className="text-2xl text-primary font-medium pb-14">
          How it works
        </h4>
        <div className="flex w-full min-w-0 flex-col justify-center">
          <Swiper
            className="w-full min-w-0"
            key={isTabletUp ? "tablet" : "mobile"}
            slidesPerView={isTabletUp ? "auto" : 1}
            centeredSlides={isTabletUp}
            spaceBetween={10}
            ref={sliderRef}
            loop={false}
          >
            {isTabletUp
              ? HowItWorksCarouselContent.map((props, idx) => (
                  <SwiperSlide key={idx} style={{ width: "fit-content" }}>
                    <img
                      src={`${baseUrl}${props.src}`}
                      alt={props.alt}
                      className="max-h-[294px] rounded-lg"
                    />
                  </SwiperSlide>
                ))
              : HowItWorksCarouselContent.map((props, idx) => (
                  <SwiperSlide key={idx}>
                    <img
                      src={`${baseUrl}${props.mobileSrc}`}
                      alt={props.alt}
                      className="mx-auto h-auto w-full max-w-[382px] rounded-lg"
                    />
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
