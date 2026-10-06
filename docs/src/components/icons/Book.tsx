import { SVGProps } from "react";
const Book = (props: SVGProps<SVGSVGElement>) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width={38}
    height={34}
    viewBox="0 0 38 34"
    fill="none"
    {...props}
  >
    <path
      stroke="currentColor"
      strokeLinecap="round"
      strokeLinejoin="round"
      strokeWidth={1.5}
      d="M18.75 5.87c0-2.828-2.302-5.12-5.143-5.12H.75v25.6h12.857c2.84 0 5.143 2.292 5.143 5.12m0-25.6c0-2.828 2.302-5.12 5.143-5.12H36.75v25.6H23.893c-2.84 0-5.143 2.292-5.143 5.12m0-25.6v24.32m0 1.28v1.28"
    />
  </svg>
);
export default Book;
