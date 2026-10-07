import { FC } from "react";
import SectionHeading from "./components/SectionHeading";
import { FastAndFrequentActivityContent } from "../../../docs/homepage/fast-and-frequent-activity";

const FastAndFrequentActivity: FC = () => {
  const { label, title, description, projects } =
    FastAndFrequentActivityContent;
  return (
    <section
      id="fast-and-frequent-activity"
      className="fast-and-frequent-activity"
    >
      <div className="pageContainer">
        <div className="fast-and-frequent-activity__inner">
          <SectionHeading
            label={label}
            title={title}
            description={description}
            mode="light"
          />
          <ul className="fast-and-frequent-activity__projects">
            {projects.map((project) => (
              <li
                key={project.title}
                className="fast-and-frequent-activity__project"
              >
                {project.logo}
                <div className="fast-and-frequent-activity__project-text">
                  <h3 className="card-title">{project.title}</h3>
                  <p className="section-description">{project.description}</p>
                </div>
              </li>
            ))}
          </ul>
        </div>
      </div>
    </section>
  );
};

export default FastAndFrequentActivity;
