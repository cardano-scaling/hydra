import { SVGProps } from "react";
const PlusExpand = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={32}
    height={32}
    viewBox="0 0 32 32"
    fill="none"
    {...props}
  >
    <rect
      width={31}
      height={31}
      x={0.5}
      y={0.5}
      stroke="currentColor"
      strokeOpacity={0.5}
      rx={15.5}
    />
    <path
      stroke="currentColor"
      strokeLinecap="round"
      strokeWidth={1.5}
      d="M16 10v12m-6-6h12"
    />
  </svg>
);
export default PlusExpand;
