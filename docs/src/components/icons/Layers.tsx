import { SVGProps } from "react";
const Layers = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={30}
    height={32}
    viewBox="0 0 30 32"
    fill="none"
    {...props}
  >
    <path
      stroke="currentColor"
      strokeLinecap="round"
      strokeLinejoin="round"
      strokeWidth={1.5}
      d="m28.75 15.689-14 7.844-14-7.844m28 6.047-14 7.845-14-7.845m0-11.472 14-7.845 14 7.845-14 7.844-14-7.844Z"
    />
  </svg>
);
export default Layers;
