import React, { FC } from "react";

type Props = {
  label: string;
  title: string;
  description?: string;
};

const SectionHeading: FC<Props> = ({ label, title, description }) => {
  return (
    <div className="section-heading">
      <span className="section-label">{label}</span>
      <h2 className="section-title">{title}</h2>
      {description && <p className="section-description">{description}</p>}
    </div>
  );
};

export default SectionHeading;
