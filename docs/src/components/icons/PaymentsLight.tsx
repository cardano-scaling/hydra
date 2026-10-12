import { SVGProps } from "react";
const PaymentsLight = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={113}
    height={118}
    viewBox="0 0 113 118"
    fill="none"
    {...props}
  >
    <ellipse
      cx={56.464}
      cy={17.893}
      fill="url(#pal-a)"
      stroke="#000"
      strokeWidth={1.5}
      rx={55.714}
      ry={17.143}
    />
    <path
      stroke="#000"
      strokeOpacity={0.25}
      strokeWidth={1.5}
      d="M112.179 17.893v15c0 9.468-24.945 17.143-55.715 17.143C25.694 50.036.75 42.36.75 32.893v-15"
    />
    <path
      stroke="#000"
      strokeDasharray="4 4"
      strokeWidth={1.5}
      d="M112.179 43.607c0 9.468-24.945 17.143-55.715 17.143C25.694 60.75.75 53.075.75 43.607"
    />
    <path
      stroke="#000"
      strokeLinecap="round"
      strokeLinejoin="round"
      strokeOpacity={0.25}
      strokeWidth={1.5}
      d="M56.464 66.75v14m5.01-7-5.01 7-5.01-7"
    />
    <ellipse
      cx={56.465}
      cy={95.321}
      fill="url(#pal-b)"
      stroke="#000"
      strokeWidth={1.5}
      rx={27.857}
      ry={8.571}
    />
    <path
      stroke="#000"
      strokeOpacity={0.25}
      strokeWidth={1.5}
      d="M84.322 95.321v7.5c0 4.734-12.472 8.572-27.857 8.572-15.386 0-27.858-3.838-27.858-8.572v-7.5"
    />
    <path
      stroke="#000"
      strokeDasharray="4 4"
      strokeWidth={1.5}
      d="M84.322 108.178c0 4.734-12.472 8.572-27.857 8.572-15.386 0-27.858-3.838-27.858-8.572"
    />
    <defs>
      <linearGradient
        id="pal-a"
        x1={56.464}
        x2={56.464}
        y1={0.75}
        y2={35.036}
        gradientUnits="userSpaceOnUse"
      >
        <stop stopOpacity={0.25} />
        <stop offset={0.85} stopOpacity={0} />
      </linearGradient>
      <linearGradient
        id="pal-b"
        x1={56.465}
        x2={56.465}
        y1={86.75}
        y2={103.893}
        gradientUnits="userSpaceOnUse"
      >
        <stop stopOpacity={0.25} />
        <stop offset={0.85} stopOpacity={0} />
      </linearGradient>
    </defs>
  </svg>
);
export default PaymentsLight;
