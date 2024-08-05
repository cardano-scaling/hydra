import React, {
  ComponentPropsWithoutRef,
  FC,
  ReactEventHandler,
  useEffect,
  useRef,
  useState,
} from "react";
import clsx from "clsx";
import Square from "../../icons/Square";
import Triangle from "../../icons/Triangle";
import Dot from "../../icons/Dot";
import { CarouselContent } from "./Coursel";
import useMediaQuery from "../../../hooks/useMediaQuery";
import { forTablet } from "../../../../helpers/media-queries";

type Props = {
  description: string;
};

const CarouselEntryDescription: FC<Props> = ({ description }) => {
  return (
    <div className="flex flex-col tablet:max-w-[478px] max-w-80 tablet:pt-6 pt-14 tablet:min-h-72 min-h-[440px] tablet:order-1 order-2">
      <div className="flex flex-col tablet:gap-4 max-w-md gap-8">
        <p>{description}</p>
        <div className="flex gap-4 tablet:flex-row flex-col">
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
  onClickBullet: (nth: number) => void;
} & ComponentPropsWithoutRef<"div">;

const Controls: FC<ControlProps> = ({ showing, onClickBullet, ...props }) => {
  return (
    <div
      className="inline-flex mt-auto gap-4 tablet:pt-20 pt-14 self-center tablet:order-2 order-1"
      {...props}
    >
      {CarouselContent.map((_, index) => (
        <button onClick={() => onClickBullet(index)}>
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
  const ref = useRef<HTMLDivElement>(null);
  const scrollerRef = useRef<HTMLDivElement>(null);
  const isTabletUp = useMediaQuery(forTablet);
  const [current, setCurrent] = useState(0);
  const [scrollSnaps, setScrollSnaps] = useState<
    { left: number; width: number }[]
  >([]);

  const handleScroll: ReactEventHandler<HTMLDivElement> = (ev) => {
    const { scrollLeft } = ev.currentTarget;
    const page = scrollSnaps.findIndex(({ left }) => left >= scrollLeft);
    setCurrent(page);
  };

  const handleClickBullet = (nth: number) => {
    const snap = scrollSnaps[nth];
    if (snap && scrollerRef.current) {
      scrollerRef.current?.scrollTo({
        left: snap.left,
        behavior: "smooth",
      });
    }
  };

  useEffect(() => {
    if (!scrollerRef.current) {
      return;
    }

    function getRects(el: Element | null): DOMRect[] {
      if (!el) {
        return [];
      }

      const next = el.nextElementSibling;
      return [el.getBoundingClientRect(), ...getRects(next)];
    }

    const { offsetWidth } = scrollerRef.current;

    const styles = getComputedStyle(scrollerRef.current);
    const innerWidth =
      offsetWidth -
      parseFloat(styles.scrollPaddingLeft) -
      parseFloat(styles.scrollPaddingRight);
    const rects = getRects(scrollerRef.current.firstElementChild);

    const snaps: typeof scrollSnaps = [rects[0]];
    let prevPage = 0;
    for (let i = 0; i < rects.length; i++) {
      const { left, width } = rects[i];
      let startPage = Math.floor(left / innerWidth);
      let endPage = Math.floor((left + width) / innerWidth);

      if (startPage !== endPage) {
        snaps.push({ left, width });
        prevPage = endPage;
      } else if (endPage > prevPage) {
        snaps.push({ left, width });
        prevPage = endPage;
      }
    }
    snaps.shift();

    setScrollSnaps(snaps);
  }, []);

  return (
    <section className="bg-[#F4F5F5]">
      <div className="component relative pageContainer flex flex-col">
        <h4 className="text-2xl text-teal font-medium pb-14">How it works</h4>
        <div ref={ref} className="w-[85vw] laptop::ml-14">
          <div
            onScroll={handleScroll}
            ref={scrollerRef}
            className="auto flex-nowrap flex relative tablet:pl-12 pr-10 [>]:flex-0 snap-x snap-mandatory overflow-scroll no-scrollbar gap-4"
          >
            {isTabletUp
              ? CarouselContent.map((props, idx) => (
                  <img src={props.src} className="tablet:ml-4 ml-1" key={idx} />
                ))
              : CarouselContent.map((props, idx) => (
                  <img
                    src={props.mobileSrc}
                    className="tablet:ml-4 ml-1"
                    key={idx}
                  />
                ))}
            {scrollSnaps.map(({ left, width }) => (
              <div
                className="absolute snap-center top-0 w-[1px] h-[1px]"
                key={left}
                style={{ left, width }}
              />
            ))}
          </div>
        </div>
        <CarouselEntryDescription
          description={CarouselContent[current]?.description}
        />
        <Controls onClickBullet={handleClickBullet} showing={current} />
      </div>
    </section>
  );
};

export default ResponsiveCarousel;
