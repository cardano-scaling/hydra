import React, {
  CSSProperties,
  FC,
  useCallback,
  useEffect,
  useRef,
  useState,
} from "react";
import clsx from "clsx";
import useBaseUrl from "@docusaurus/useBaseUrl";
import { EventCallback } from "@rive-app/react-canvas";
import SectionHeading from "./components/SectionHeading";
import { RiveWrapperContainWithEvents } from "./components/RiveWrapper";
import useMediaQuery from "../../hooks/useMediaQuery";
import { forTablet } from "../../../helpers/media-queries";
import { WhatIsHydraContent } from "../../../docs/homepage/what-is-hydra";

const STEP_START_TIMES = [0, 3.26, 7.26];
const TIMELINE_DURATION = 10;

const useTimelineStep = () => {
  const [activeStep, setActiveStep] = useState(0);
  const time = useRef(0);

  const restart = useCallback(() => {
    time.current = 0;
    setActiveStep(0);
  }, []);

  const advance = useCallback<EventCallback>((event) => {
    time.current = (time.current + Number(event.data)) % TIMELINE_DURATION;
    setActiveStep(
      STEP_START_TIMES.filter((start) => time.current >= start).length - 1
    );
  }, []);

  return { activeStep, restart, advance };
};

const WhatIsHydra: FC = () => {
  const { label, title, description, graphicTitle, steps } = WhatIsHydraContent;
  const { activeStep, restart, advance } = useTimelineStep();

  return (
    <section id="what-is-hydra" className="what-is-hydra">
      <div className="pageContainer">
        <div className="what-is-hydra__inner">
          <SectionHeading
            label={label}
            title={title}
            description={description}
          />
          <div className="what-is-hydra__card">
            <ol
              className="what-is-hydra__steps"
              style={{ "--active-step": activeStep } as CSSProperties}
            >
              {steps.map((step, i) => (
                <li
                  key={step.title}
                  className={clsx(
                    "what-is-hydra__step",
                    i === activeStep && "what-is-hydra__step--active"
                  )}
                  aria-current={i === activeStep ? "step" : undefined}
                >
                  <h3 className="what-is-hydra__step-title">{step.title}</h3>
                  <p className="what-is-hydra__step-description">
                    {step.description}
                  </p>
                </li>
              ))}
            </ol>
            <div className="what-is-hydra__graphic">
              <p className="what-is-hydra__graphic-title">{graphicTitle}</p>
              <Animation onRestart={restart} onAdvance={advance} />
            </div>
          </div>
        </div>
      </div>
    </section>
  );
};

export default WhatIsHydra;

type AnimationProps = {
  onRestart: () => void;
  onAdvance: EventCallback;
};

const Animation: FC<AnimationProps> = ({ onRestart, onAdvance }) => {
  const isTabletUp = useMediaQuery(forTablet);
  const desktopSrc = useBaseUrl("/rive/open-run-settle-animation.riv");
  const mobileSrc = useBaseUrl("/rive/open-run-settle-animation-mobile.riv");
  const [isReady, setIsReady] = useState(false);
  useEffect(() => setIsReady(true), []);

  return (
    <div className="what-is-hydra__animation">
      {isReady && (
        <RiveWrapperContainWithEvents
          key={isTabletUp ? "desktop" : "mobile"}
          src={isTabletUp ? desktopSrc : mobileSrc}
          onLoad={onRestart}
          onAdvance={onAdvance}
        />
      )}
    </div>
  );
};
