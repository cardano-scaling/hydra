import React, { FC } from "react";
import clsx from "clsx";

type Props = {
  label: string;
  title: string;
  description?: string;
  mode?: "light" | "dark";
};

const SectionHeading: FC<Props> = ({
  label,
  title,
  description,
  mode = "dark",
}) => {
  return (
    <div
      className={clsx(
        "section-heading",
        mode === "light" && "section-heading--light"
      )}
    >
      <span className="section-label">{label}</span>
      <h2 className="section-title">{title}</h2>
      {description && <p className="section-description">{description}</p>}
    </div>
  );
};

export default SectionHeading;
