import * as React from "react";
import { SVGProps } from "react";

const CheckBadge = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width="20"
    height="20"
    viewBox="0 0 20 20"
    fill="none"
    {...props}
  >
    <rect width="20" height="20" rx="10" fill="white" />
    <path
      d="M5 10.3847L8.07692 13.4617L15 6.53857"
      stroke="black"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round"
    />
  </svg>
);

export default CheckBadge;
