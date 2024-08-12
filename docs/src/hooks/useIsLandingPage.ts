import { useContext } from "react";
import { PageContext, PageType } from "../context/PageContext";

export function useIsLandingPage() {
  const context = useContext(PageContext);
  return context && context.page === PageType.Landing;
}
