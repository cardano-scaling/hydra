import * as React from "react";
import { SVGProps } from "react";

const Code = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width="20"
    height="18"
    viewBox="0 0 20 18"
    fill="none"
    {...props}
  >
    <path
      d="M0.75 14.4643L7.5 7.60714L0.75 0.75M9.75 16.75H18.75"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinecap="round"
      strokeLinejoin="round"
    />
  </svg>
);
export default Code;
