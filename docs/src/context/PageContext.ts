import { createContext } from "react";

export enum PageType {
  Landing = "Landing",
  Default = "Default",
}

export const PageContext = createContext({
  page: PageType.Default,
});
