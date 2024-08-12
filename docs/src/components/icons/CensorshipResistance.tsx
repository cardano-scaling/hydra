import * as React from "react";
import { SVGProps } from "react";
const CensorshipResistance = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width="24"
    height="24"
    viewBox="0 0 24 24"
    fill="none"
    {...props}
  >
    <mask
      id="mask0_1761_1389"
      maskUnits="userSpaceOnUse"
      x="0"
      y="0"
      width="24"
      height="24"
    >
      <rect width="24" height="24" fill="#D9D9D9" />
    </mask>
    <g mask="url(#mask0_1761_1389)">
      <path
        d="M10.95 14.8569L15.9078 9.89917L15.1943 9.18567L10.95 13.4299L8.8115 11.2914L8.098 12.0049L10.95 14.8569ZM12 20.9607C9.991 20.3645 8.32208 19.1475 6.99325 17.3097C5.66442 15.4718 5 13.4017 5 11.0992V5.69142L12 3.07617L19 5.69142V11.0992C19 13.4017 18.3356 15.4718 17.0068 17.3097C15.6779 19.1475 14.009 20.3645 12 20.9607ZM12 19.8992C13.7333 19.3492 15.1667 18.2492 16.3 16.5992C17.4333 14.9492 18 13.1158 18 11.0992V6.37417L12 4.14342L6 6.37417V11.0992C6 13.1158 6.56667 14.9492 7.7 16.5992C8.83333 18.2492 10.2667 19.3492 12 19.8992Z"
        fill="#283032"
      />
    </g>
  </svg>
);
export default CensorshipResistance;
