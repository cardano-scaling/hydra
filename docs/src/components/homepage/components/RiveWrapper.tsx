"use client";

import Rive, {
  Fit,
  Layout,
  RiveParameters,
  RiveProps,
  useRive,
} from "@rive-app/react-canvas";
import React from "react";

export const RiveWrapper: React.FC<RiveProps> = (props) => {
  return <Rive {...props} />;
};

export const RiveWrapperContain: React.FC<Omit<RiveProps, "layout">> = (
  props
) => {
  return <Rive {...props} layout={new Layout({ fit: Fit.Contain })} />;
};

export const RiveWrapperCover: React.FC<Omit<RiveProps, "layout">> = (
  props
) => {
  return <Rive {...props} layout={new Layout({ fit: Fit.Cover })} />;
};

type RivePlaybackCallbacks = Pick<RiveParameters, "onLoad" | "onAdvance">;

export const RiveWrapperContainWithEvents: React.FC<
  { src: string } & RivePlaybackCallbacks
> = (params) => {
  const { RiveComponent } = useRive({
    ...params,
    autoplay: true,
    layout: new Layout({ fit: Fit.Contain }),
  });
  return <RiveComponent />;
};
