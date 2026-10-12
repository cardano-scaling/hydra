import { SVGProps } from "react";
const AgentsLight = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={285}
    height={108}
    viewBox="0 0 285 108"
    fill="none"
    {...props}
  >
    <path
      stroke="#000"
      strokeWidth={1.5}
      d="M94.136 30.162C94.136 37.804 77.873 44 57.812 44c-20.061 0-36.324-6.196-36.324-13.838 0-7.642 16.263-13.838 36.324-13.838 20.06 0 36.324 6.196 36.324 13.838Z"
    />
    <path
      stroke="#000"
      strokeDasharray="4 4"
      strokeWidth={1.5}
      d="M94.136 30.161v27.676c0 7.642-16.263 13.838-36.324 13.838-20.061 0-36.324-6.196-36.324-13.838V30.16M99.325 43.998s22.335-30.27 42.81-30.27c20.476 0 42.81 30.27 42.81 30.27"
    />
    <path
      stroke="#000"
      strokeWidth={1.5}
      d="M264.512 30.162c0 7.642-16.263 13.838-36.324 13.838-20.061 0-36.324-6.196-36.324-13.838 0-7.642 16.263-13.838 36.324-13.838 20.061 0 36.324 6.196 36.324 13.838Z"
    />
    <path
      stroke="#000"
      strokeDasharray="4 4"
      strokeWidth={1.5}
      d="M264.512 30.161v27.676c0 7.642-16.263 13.838-36.324 13.838-20.061 0-36.324-6.196-36.324-13.838V30.16"
    />
    <circle cx={142.568} cy={14.594} r={6.919} fill="#000" />
    <path
      fill="url(#al-a)"
      fillOpacity={0.25}
      d="M115 91.337c0 9.113-25.744 16.5-57.5 16.5S0 100.45 0 91.337c0-9.112 25.744-16.5 57.5-16.5s57.5 7.388 57.5 16.5Z"
    />
    <path
      fill="url(#al-b)"
      fillOpacity={0.25}
      d="M285 91.337c0 9.113-25.744 16.5-57.5 16.5s-57.5-7.387-57.5-16.5c0-9.112 25.744-16.5 57.5-16.5s57.5 7.388 57.5 16.5Z"
    />
    <defs>
      <radialGradient
        id="al-a"
        cx={0}
        cy={0}
        r={1}
        gradientTransform="matrix(0 16.5 -57.5 0 57.5 91.337)"
        gradientUnits="userSpaceOnUse"
      >
        <stop />
        <stop offset={1} stopOpacity={0} />
      </radialGradient>
      <radialGradient
        id="al-b"
        cx={0}
        cy={0}
        r={1}
        gradientTransform="matrix(0 16.5 -57.5 0 227.5 91.337)"
        gradientUnits="userSpaceOnUse"
      >
        <stop />
        <stop offset={1} stopOpacity={0} />
      </radialGradient>
    </defs>
  </svg>
);
export default AgentsLight;
