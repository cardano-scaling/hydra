import React, { FC } from "react";

type Props = {
  label: string;
  title: string;
};

const SectionHeading: FC<Props> = ({ label, title }) => {
  return (
    <div className="section-heading">
      <span className="section-label">{label}</span>
      <h2 className="section-title">{title}</h2>
    </div>
  );
};

export default SectionHeading;
