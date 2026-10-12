import { SVGProps } from "react";
const GovLight = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={79}
    height={82}
    viewBox="0 0 79 82"
    fill="none"
    {...props}
  >
    <path
      stroke="#000"
      strokeWidth={1.5}
      d="M26.6 20.614H5.104L.75 39.934V80.75h77.279V39.934l-4.354-19.32H52.18m25.85 19.32H.75"
    />
    <path fill="url(#gl-a)" d="M26.328.75h26.123v29.388H26.328V.75Z" />
    <path
      stroke="#000"
      strokeWidth={1.5}
      d="M21.975 30.138h35.102M26.328.75h26.123v29.388H26.328V.75Z"
    />
    <path
      stroke="#000"
      strokeLinecap="round"
      strokeLinejoin="round"
      strokeWidth={1.5}
      d="m32.587 16.774 4.019 4.112 9.042-9.252"
    />
    <defs>
      <linearGradient
        id="gl-a"
        x1={39.526}
        x2={39.526}
        y1={0.75}
        y2={30.138}
        gradientUnits="userSpaceOnUse"
      >
        <stop stopOpacity={0.25} />
        <stop offset={0.85} stopOpacity={0} />
      </linearGradient>
    </defs>
  </svg>
);
export default GovLight;
