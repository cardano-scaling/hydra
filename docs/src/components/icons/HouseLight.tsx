import { SVGProps } from "react";
const HouseLight = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={161}
    height={149}
    viewBox="0 0 161 149"
    fill="none"
    {...props}
  >
    <path
      stroke="#000"
      strokeDasharray="3 3"
      strokeWidth={1.5}
      d="m130.096 58.177 28.538 12.888-81.538 47L1.64 77.812l31.997-15.873"
    />
    <path
      fill="url(#hol-a)"
      d="m77.096 118.065 81.538-47-28.538-12.888V73.94l-36.46 21-9.5-5.5-14 7.5-36.5-19.127V61.939L1.64 77.811l75.457 40.253Z"
    />
    <path
      fill="#EDE5EF"
      d="m32.91 49.76-5.235.524 51.832-21.981 32.985-6.021 20.157 20.672-2.994 1.652v29.238L93.382 94.787l-8.9-5.498-14.137 8.116-25.13-12.83-7.593-3.875-4.712-2.405V49.76Z"
    />
    <path
      stroke="#000"
      strokeLinecap="round"
      strokeLinejoin="round"
      strokeWidth={1.5}
      d="M32.91 49.76v28.535l4.713 2.405m32.722 16.705V61.017M37.623 80.7l7.591 3.876 25.131 12.829 14.136-8.115m8.9-24.671 36.274-20.013 2.994-1.652-20.157-20.672-32.985 6.02-51.832 21.982 31.413-3.141m0 0 53.404-24.861m-53.404 24.86 20.42 25.132 13.874-7.655m-8.9 24.67V69.657m45.173-25.05v29.238L93.382 94.787l-8.9-5.498m8.9 5.498V64.619M37.622 80.7V63.635l7.592 3.665v17.276m68.325-24.064-10.733 6.003v13.874l10.733-6.367m0-13.51 4.712-2.636v13.35l-4.712 2.796m0-13.51v13.51m-63.09 1.393v-13.35l14.66 7.416v13.264l-14.66-7.33Z"
    />
    <defs>
      <linearGradient
        id="hol-a"
        x1={131.957}
        x2={28.316}
        y1={35.532}
        y2={113.344}
        gradientUnits="userSpaceOnUse"
      >
        <stop stopOpacity={0.25} />
        <stop offset={0.85} stopOpacity={0} />
      </linearGradient>
    </defs>
  </svg>
);
export default HouseLight;
