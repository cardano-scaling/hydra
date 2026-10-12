import { SVGProps } from "react";
const PredictionsLight = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={147}
    height={102}
    viewBox="0 0 147 102"
    fill="none"
    {...props}
  >
    <path
      stroke="#000"
      strokeDasharray="4 4"
      strokeWidth={1.5}
      d="M0 100.75c41.525 0 52.542-100 72.881-100 20.34 0 31.356 100 72.882 100"
    />
    <path
      fill="url(#prl-a)"
      d="M73.729 26.174c-26.271 0-31.356 74.576-72.881 74.576H146.61c-41.525 0-46.61-74.576-72.881-74.576Z"
    />
    <defs>
      <linearGradient
        id="prl-a"
        x1={73.729}
        x2={73.729}
        y1={26.174}
        y2={100.75}
        gradientUnits="userSpaceOnUse"
      >
        <stop stopOpacity={0.25} />
        <stop offset={0.85} stopOpacity={0} />
      </linearGradient>
    </defs>
  </svg>
);
export default PredictionsLight;
