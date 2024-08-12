import { useEffect, useState } from "react";

export default function useMediaQuery(query: string) {
  const [match, setMatch] = useState<boolean>(false);

  useEffect(() => {
    const matchMedia = window.matchMedia(query);
    const listener = (e: MediaQueryListEvent) => {
      if (process.env.NODE_ENV !== "production") {
        console.info(`Media query change: ${query}`, e.matches);
      }
      setMatch(e.matches);
    };
    matchMedia.addEventListener("change", listener);
    setMatch(matchMedia.matches);
    return () => matchMedia.removeEventListener("change", listener);
  }, [query]);
  return match;
}
