import * as React from "react";
import { SVGProps } from "react";
const Square = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width="16"
    height="16"
    viewBox="0 0 16 16"
    fill="none"
    {...props}
  >
    <path d="M15.35 0.324219H0V15.6742H15.35V0.324219Z" fill="#722042" />
  </svg>
);
export default Square;
