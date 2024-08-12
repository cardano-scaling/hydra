import * as React from "react";
import { SVGProps } from "react";
const Triangle = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width="20"
    height="18"
    viewBox="0 0 20 18"
    fill="none"
    {...props}
  >
    <path
      d="M9.89156 0.744141L0.351562 17.2541H19.4216L9.89156 0.744141Z"
      fill="#C8799A"
    />
  </svg>
);
export default Triangle;
