import { FC, Fragment, useCallback, useEffect, useRef, useState } from "react";
import clsx from "clsx";
import { useInView, useReducedMotion } from "framer-motion";

export type TerminalLine =
  | { type: "comment" | "command"; text: string }
  | { type: "output"; text: string; typewriter?: boolean }
  | { type: "blank" };

type Props = {
  title: string;
  lines: TerminalLine[];
};

const TYPEWRITER_DELAY_MS = 400;
const TYPEWRITER_INTERVAL_MS = 50;

const isTypewriterLine = (line: TerminalLine) =>
  line.type === "output" && Boolean(line.typewriter);

const Terminal: FC<Props> = ({ title, lines }) => {
  const [isTyped, setIsTyped] = useState(!lines.some(isTypewriterLine));
  const markTyped = useCallback(() => setIsTyped(true), []);

  return (
    <div className="terminal">
      <div className="terminal__bar">
        <div className="terminal__dots" aria-hidden="true">
          <span className="terminal__dot" />
          <span className="terminal__dot" />
          <span className="terminal__dot" />
        </div>
        <span className="section-label terminal__title">{title}</span>
      </div>
      <pre className="terminal__code">
        <code>
          {lines.map((line, i) => (
            <Fragment key={i}>
              {line.type === "output" && line.typewriter ? (
                <TypewriterOutput text={line.text} onDone={markTyped} />
              ) : (
                <TerminalLineContent line={line} />
              )}
              {"\n"}
            </Fragment>
          ))}
          <span
            className={clsx(
              "terminal__last-line",
              !isTyped && "terminal__last-line--waiting"
            )}
          >
            <Prompt />
            {isTyped && (
              <span className="terminal__cursor" aria-hidden="true" />
            )}
          </span>
        </code>
      </pre>
    </div>
  );
};

export default Terminal;

const Prompt: FC = () => <span className="terminal__prompt">$ </span>;

const TerminalLineContent: FC<{ line: TerminalLine }> = ({ line }) => {
  switch (line.type) {
    case "comment":
      return <span className="terminal__comment">{line.text}</span>;
    case "output":
      return <span className="terminal__output">{line.text}</span>;
    case "command": {
      const [command, ...args] = line.text.split(" ");
      return (
        <>
          <Prompt />
          <span className="terminal__command">{command}</span>
          {args.length > 0 && ` ${args.join(" ")}`}
        </>
      );
    }
    default:
      return null;
  }
};

const TypewriterOutput: FC<{ text: string; onDone: () => void }> = ({
  text,
  onDone,
}) => {
  const ref = useRef<HTMLSpanElement>(null);
  const isInView = useInView(ref, { once: true, amount: "all" });
  const reduceMotion = useReducedMotion();
  const [typedCount, setTypedCount] = useState(0);

  useEffect(() => {
    if (!isInView) {
      return;
    }
    if (reduceMotion) {
      setTypedCount(text.length);
      return;
    }
    let typed = 0;
    let interval: number | undefined;
    const timeout = window.setTimeout(() => {
      interval = window.setInterval(() => {
        typed += 1;
        setTypedCount(typed);
        if (typed >= text.length) {
          window.clearInterval(interval);
        }
      }, TYPEWRITER_INTERVAL_MS);
    }, TYPEWRITER_DELAY_MS);
    return () => {
      window.clearTimeout(timeout);
      window.clearInterval(interval);
    };
  }, [isInView, reduceMotion, text]);

  const isDone = typedCount >= text.length;
  useEffect(() => {
    if (isDone) {
      onDone();
    }
  }, [isDone, onDone]);

  return (
    <span ref={ref} className="terminal__output">
      <span aria-hidden="true">
        {text.slice(0, typedCount)}
        <span className="terminal__untyped">{text.slice(typedCount)}</span>
      </span>
      <span className="sr-only">{text}</span>
    </span>
  );
};
