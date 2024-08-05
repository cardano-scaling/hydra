import type { Config } from "tailwindcss";

const config: Config = {
  content: ["./src/**/*.{js,jsx,ts,tsx}"],
  theme: {
    backgroundImage: {},
    screens: {
      phablet: "640px",
      // => @media (min-width: 640px) { ... }

      tablet: "768px",
      // => @media (min-width: 768px) { ... }

      laptop: "1024px",
      // => @media (min-width: 1024px) { ... }

      desktop: "1280px",
      // => @media (min-width: 1280px) { ... }
    },
    fontFamily: {
      display: [
        "Bw Gradual",
        "ui-sans-serif",
        "system-ui",
        "sans-serif",
        '"Apple Color Emoji"',
        '"Segoe UI Emoji"',
        '"Segoe UI Symbol"',
        '"Noto Color Emoji"',
      ],
      body: [
        "var(--font-lexend)",
        "ui-sans-serif",
        "system-ui",
        "sans-serif",
        '"Apple Color Emoji"',
        '"Segoe UI Emoji"',
        '"Segoe UI Symbol"',
        '"Noto Color Emoji"',
      ],
    },
    colors: {
      transparent: "transparent",
      current: "currentColor",
      red: "#F00000",
      darkRed: "#721F41",
      yellow: "#FFF500",
      violet: "#00006D",
      teal: {
        DEFAULT: "#12506C",
        dark: "#08202B",
        light: "#7196A7",
        lightest: "#B8CBD3",
      },
      blue: {},
      black: "#283032",
      "true-black": "#000000",
      gray: {
        DEFAULT: "#D4D6D6",
      },
      white: "#FFFFFF",
    },
  },
  plugins: [],
};
export default config;
