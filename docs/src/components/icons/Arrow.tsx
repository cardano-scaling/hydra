import * as React from "react";
import { SVGProps } from "react";
const Arrow = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width="20"
    height="20"
    viewBox="0 0 20 20"
    fill="none"
    {...props}
  >
    <rect
      x="0.5"
      y="0.5"
      width="19"
      height="19"
      rx="9.5"
      stroke="currentColor"
    />
    <path
      d="M13.0847 10.5H5V9.5H13.0847L9.28717 5.7025L10 5L15 10L10 15L9.28717 14.2975L13.0847 10.5Z"
      fill="currentColor"
    />
  </svg>
);
export default Arrow;
