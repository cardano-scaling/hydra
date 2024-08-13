export const not = (query: string) => "not screen and " + query;

export const forPhablet = "(min-width: 640px)";

export const forTablet = "(min-width: 768px)";

export const forLaptop = "(min-width: 1024px)";

export const forDesktop = "(min-width: 1280px)";

export const forTabletOnly = `${forTablet} and (max-width: 1024px)`;
