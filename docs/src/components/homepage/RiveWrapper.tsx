"use client";

import Rive, { Fit, Layout, RiveProps } from "@rive-app/react-canvas";
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
