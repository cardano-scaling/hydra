import { SVGProps } from "react";
const DotBox = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={32}
    height={32}
    viewBox="0 0 32 32"
    fill="none"
    {...props}
  >
    <g fill="currentColor" clipPath="url(#dot-box-a)">
      <path d="M4 2a2 2 0 1 1-4 0 2 2 0 0 1 4 0ZM18 2a2 2 0 1 1-4 0 2 2 0 0 1 4 0ZM32 2a2 2 0 1 1-4 0 2 2 0 0 1 4 0ZM4 16a2 2 0 1 1-4 0 2 2 0 0 1 4 0ZM18 16a2 2 0 1 1-4 0 2 2 0 0 1 4 0ZM32 16a2 2 0 1 1-4 0 2 2 0 0 1 4 0ZM4 30a2 2 0 1 1-4 0 2 2 0 0 1 4 0ZM18 30a2 2 0 1 1-4 0 2 2 0 0 1 4 0ZM32 30a2 2 0 1 1-4 0 2 2 0 0 1 4 0Z" />
    </g>
    <defs>
      <clipPath id="dot-box-a">
        <path fill="currentColor" d="M0 0h32v32H0z" />
      </clipPath>
    </defs>
  </svg>
);
export default DotBox;
