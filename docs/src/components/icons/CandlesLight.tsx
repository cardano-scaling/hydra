import { SVGProps } from "react";
const CandlesLight = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={122}
    height={100}
    viewBox="0 0 122 100"
    fill="none"
    {...props}
  >
    <path
      stroke="#000"
      strokeOpacity={0.25}
      strokeWidth={1.5}
      d="M43.343 14.815v77.779M78.528 0v81.482M10.01 7.407V100M111.861 14.815v55.556"
    />
    <path fill="#EDE5EF" d="M.75 30.556h18.518v44.445H.75z" />
    <path fill="url(#cal-a)" d="M.75 30.556h18.518v44.445H.75z" />
    <path stroke="#000" strokeWidth={1.5} d="M.75 30.556h18.518v44.445H.75z" />
    <path fill="#EDE5EF" d="M69.269 15.741h18.518v44.445H69.269z" />
    <path fill="url(#cal-b)" d="M69.269 15.741h18.518v44.445H69.269z" />
    <path
      stroke="#000"
      strokeWidth={1.5}
      d="M69.269 15.741h18.518v44.445H69.269z"
    />
    <path
      stroke="#000"
      strokeDasharray="4 4"
      strokeWidth={1.5}
      d="M103.352 22.047h17.018v29.982h-17.018zM34.833 40.566h17.018v33.685H34.833z"
    />
    <defs>
      <linearGradient
        id="cal-a"
        x1={10.009}
        x2={10.009}
        y1={30.556}
        y2={75.001}
        gradientUnits="userSpaceOnUse"
      >
        <stop stopOpacity={0.25} />
        <stop offset={0.85} stopOpacity={0} />
      </linearGradient>
      <linearGradient
        id="cal-b"
        x1={78.528}
        x2={78.528}
        y1={15.741}
        y2={60.185}
        gradientUnits="userSpaceOnUse"
      >
        <stop stopOpacity={0.25} />
        <stop offset={0.85} stopOpacity={0} />
      </linearGradient>
    </defs>
  </svg>
);
export default CandlesLight;
