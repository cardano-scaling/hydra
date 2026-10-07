import * as React from "react";
import { SVGProps } from "react";
const Arrow = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={20}
    height={20}
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

export const RightArrow = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={20}
    height={20}
    viewBox="0 0 20 20"
    fill="none"
    {...props}
  >
    <path
      d="M0.769231 10H19.2308M10 0.769231L19.2308 10L10 19.2308"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round"
    />
  </svg>
);

export const ExternalArrow = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={16}
    height={16}
    viewBox="0 0 16 16"
    fill="none"
    {...props}
  >
    <path
      d="M2.34338 13.6569L13.6571 2.34315M2.34338 2.34315H13.6571V13.6569"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round"
    />
  </svg>
);

export const DownUpArrows = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={37}
    height={48}
    viewBox="0 0 37 48"
    fill="none"
    {...props}
  >
    <path
      d="M7.4122 0.712674L7.41221 47.0798M14.4681 40.0239L7.41221 47.0798L0.356338 40.0239"
      stroke="currentColor"
    />
    <path
      d="M29.5878 47.0798L29.5878 0.712674M22.5319 7.76854L29.5878 0.712674L36.6437 7.76854"
      stroke="currentColor"
    />
  </svg>
);

export default Arrow;
