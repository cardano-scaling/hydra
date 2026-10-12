import { FC, ReactNode, useId, useState } from "react";
import SectionHeading from "./components/SectionHeading";
import { UseCasesContent } from "../../../docs/homepage/use-cases";
import LargeLink from "./components/LargeLink";
import { Badge } from "./components/Badge";
import PlusExpand from "../icons/PlusExpand";
import useBaseUrl from "@docusaurus/useBaseUrl";

const UseCases: FC = () => {
  const { label, title, text, items, cta } = UseCasesContent;
  const ctaUrl = useBaseUrl(cta.url);
  return (
    <section id="use-cases">
      <div className="pageContainer">
        <div className="padding-section flex flex-col gap-8 desktop:gap-14">
          <SectionHeading
            label={label}
            title={title}
            description={text}
            mode="light"
          />
          <div className="grid grid-cols-1 gap-2 phablet:grid-cols-2 desktop:gap-4 desktop:grid-cols-4">
            {items.map((item, index) => (
              <UseCaseCard
                key={item.label + index}
                icon={item.icon}
                iconHover={item.iconHover}
                title={item.title}
                label={item.label}
                labelIcon={item.labelIcon}
                text={item.text}
                isFirst={index === 0}
              />
            ))}
          </div>
          <LargeLink to={ctaUrl} mode="light">
            {cta.label}
          </LargeLink>
        </div>
      </div>
    </section>
  );
};

export default UseCases;

type UseCaseCardProps = {
  icon: ReactNode;
  iconHover: ReactNode;
  title: string;
  label: string;
  labelIcon: ReactNode;
  text: string;
  isFirst?: boolean;
};

const UseCaseCard = ({
  icon,
  iconHover,
  title,
  label,
  labelIcon,
  text,
  isFirst,
}: UseCaseCardProps) => {
  const [open, setOpen] = useState(false);
  const id = useId();
  const panelId = `${id}-panel`;
  const titleId = `${id}-title`;

  const inertProps = open ? {} : ({ inert: "" } as Record<string, string>);

  return (
    <article
      aria-labelledby={titleId}
      className={`group relative use-case-card overflow-hidden ${
        isFirst ? "desktop:col-span-2" : ""
      }`}
    >
      {/* Decorative icons: hidden from assistive tech */}
      <div
        aria-hidden="true"
        className="relative grid place-items-center w-full min-h-[9.875rem] tablet:min-h-[12.875rem]"
      >
        <span className="absolute transition-opacity motion-reduce:transition-none group-hover:opacity-0 [&_svg]:max-w-full [&_svg]:h-auto">
          {icon}
        </span>
        <span className="absolute transition-opacity motion-reduce:transition-none opacity-0 group-hover:opacity-100 [&_svg]:max-w-full [&_svg]:h-auto">
          {iconHover}
        </span>
      </div>

      <h3 id={titleId} className="use-case-card__title mt-auto">
        {title}
      </h3>

      <Badge
        icon={labelIcon}
        className="transition-colors motion-reduce:transition-none group-hover:border-white/25 group-hover:text-white"
      >
        {label}
      </Badge>

      {/* Overlay panel */}
      <div
        id={panelId}
        role="region"
        aria-labelledby={titleId}
        // `inert` removes the hidden panel from the tab order and the
        // accessibility tree while it's off-screen
        {...inertProps}
        className={`absolute inset-0 grid place-items-center text-center bg-[var(--hydra-black)] text-[var(--pure-white)] p-8 transition-transform motion-reduce:transition-none ${
          open ? "translate-x-0" : "translate-x-full"
        }`}
      >
        <p className="max-w-[25rem]">{text}</p>
      </div>

      <button
        type="button"
        aria-expanded={open}
        aria-controls={panelId}
        aria-label={`${open ? "Hide" : "Show"} details: ${title}`}
        onClick={() => setOpen((prev) => !prev)}
        onKeyDown={(e) => {
          if (e.key === "Escape" && open) setOpen(false);
        }}
        className="absolute top-4 right-4 z-10"
      >
        <PlusExpand
          aria-hidden="true"
          focusable="false"
          className={`transition-all motion-reduce:transition-none ${
            open ? "text-[var(--pure-white)] rotate-45" : ""
          }`}
        />
      </button>
    </article>
  );
};
