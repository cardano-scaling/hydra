import { useLayoutEffect, useState } from "react";
import { debounce } from "../../helpers/debounce";

export function useWindowSize(debounceTime?: number) {
  const [size, setSize] = useState<{
    width: number | null;
    height: number | null;
  }>({
    width: null,
    height: null,
  });

  useLayoutEffect(() => {
    const handleResize = () => {
      setSize({
        width: window.innerWidth,
        height: window.innerHeight,
      });
    };

    // Set initial size
    handleResize();

    const onResize = debounceTime
      ? debounce(handleResize, debounceTime)
      : handleResize;

    window.addEventListener("resize", onResize);

    return () => {
      window.removeEventListener("resize", onResize);
    };
  }, []);

  return size;
}
