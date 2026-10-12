import { ReactNode } from "react";
import Thunder from "../../icons/Thunder";

type Props = {
  children?: ReactNode;
  icon?: ReactNode;
  mode?: "light" | "dark";
  className?: string;
};

export const Badge = ({
  children,
  icon = <Thunder />,
  mode = "light",
  className,
}: Props) => {
  return (
    <p
      className={`flex items-center gap-2 w-fit py-1 px-2.5 rounded border ${
        mode === "dark"
          ? "border-white/25 text-white"
          : "border-black/25 text-[var(--pure-black)]"
      } ${className}`}
    >
      {icon && <span aria-hidden="true">{icon}</span>}
      <span className="badge-title">{children}</span>
    </p>
  );
};
