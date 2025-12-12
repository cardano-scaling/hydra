/** @type {import('tailwindcss').Config} */
export default {
  content: [
    "./index.html",
    "./src/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      colors: {
        dark: {
          bg: '#0a0e14',
          card: '#12171f',
          hover: '#1a2029',
          input: '#0d1117',
        },
        border: '#2d3748',
        accent: {
          DEFAULT: '#4fd1c5',
          hover: '#38b2ac',
        },
        success: '#48bb78',
        warning: '#ed8936',
        danger: '#fc8181',
        info: '#63b3ed',
      },
      fontFamily: {
        mono: ['JetBrains Mono', 'Fira Code', 'SF Mono', 'monospace'],
      },
    },
  },
  plugins: [],
}

