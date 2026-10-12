import { SVGProps } from "react";
const GovDark = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={79}
    height={81}
    viewBox="0 0 79 81"
    fill="none"
    {...props}
  >
    <path
      stroke="url(#gd-a)"
      d="M26.35 20.364H4.854L.5 39.684V80.5h77.279V39.684l-4.354-19.32H51.93m25.85 19.32H.5"
    />
    <path fill="url(#gd-b)" d="M26.078.5h26.123v29.388H26.078V.5Z" />
    <path
      stroke="url(#gd-c)"
      d="M21.725 29.888h35.102M26.078.5h26.123v29.388H26.078V.5Z"
    />
    <path
      stroke="url(#gd-d)"
      strokeLinecap="round"
      strokeLinejoin="round"
      d="m32.337 16.524 4.019 4.112 9.042-9.252"
    />
    <defs>
      <linearGradient
        id="gd-a"
        x1={39.139}
        x2={106.375}
        y1={20.364}
        y2={79.67}
        gradientUnits="userSpaceOnUse"
      >
        <stop stopColor="#D7263D" />
        <stop offset={1} stopColor="#8A38F5" />
      </linearGradient>
      <linearGradient
        id="gd-b"
        x1={39.276}
        x2={39.276}
        y1={0.5}
        y2={29.888}
        gradientUnits="userSpaceOnUse"
      >
        <stop stopOpacity={0.25} />
        <stop offset={0.85} stopOpacity={0} />
      </linearGradient>
      <linearGradient
        id="gd-c"
        x1={39.276}
        x2={71.749}
        y1={0.5}
        y2={27.124}
        gradientUnits="userSpaceOnUse"
      >
        <stop stopColor="#D7263D" />
        <stop offset={1} stopColor="#8A38F5" />
      </linearGradient>
      <linearGradient
        id="gd-d"
        x1={38.867}
        x2={49.288}
        y1={11.384}
        y2={21.482}
        gradientUnits="userSpaceOnUse"
      >
        <stop stopColor="#D7263D" />
        <stop offset={1} stopColor="#8A38F5" />
      </linearGradient>
    </defs>
  </svg>
);
export default GovDark;
