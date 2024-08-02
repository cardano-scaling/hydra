// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require("prism-react-renderer/themes/github");
const darkCodeTheme = require("prism-react-renderer/themes/dracula");
const docsMetadataJson = require("./static/docs-metadata.json");

const customFields = {
  apiSpecDir: "../hydra-node/json-schemas",
  apiSpecUrl: "api.yaml",
};

const editUrl = "https://github.com/cardano-scaling/hydra/tree/master/docs";

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Hydra Head protocol documentation",
  url: "https://input-output-hk.github.io",
  baseUrl: "/head-protocol/",
  onBrokenLinks: "throw",
  onBrokenMarkdownLinks: "warn",
  favicon: "img/hydra.png",
  organizationName: "Input Output",
  projectName: "Hydra",
  staticDirectories: ["static", customFields.apiSpecDir],
  customFields,

  scripts: [
    {
      src: "https://plausible.io/js/script.js",
      defer: true,
      "data-domain": "hydra.family",
    },
  ],

  presets: [
    [
      "classic",
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          editUrl,
          editLocalizedFiles: true,
          sidebarPath: require.resolve("./sidebars.js"),
          sidebarCollapsible: false,
        },
        blog: {
          path: "adr",
          routeBasePath: "/adr",
          blogTitle: "Architectural Decision Records",
          blogDescription:
            "Lightweight technical documentation for the Hydra node software.",
          blogSidebarTitle: "Architectural Decision Records",
          blogSidebarCount: "ALL",
          sortPosts: "ascending",
          authorsMapPath: "../authors.yaml",
        },
        theme: {
          customCss: require.resolve("./src/css/custom.css"),
        },
      }),
    ],
  ],

  plugins: [
    async function myPlugin(context, options) {
      return {
        name: "docusaurus-tailwindcss",
        configurePostCss(postcssOptions) {
          // Appends TailwindCSS and AutoPrefixer.
          postcssOptions.plugins.push(require("tailwindcss"));
          postcssOptions.plugins.push(require("autoprefixer"));
          return postcssOptions;
        },
      };
    },
    [
      "content-docs",
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: "standalone",
        path: "standalone",
        routeBasePath: "/",
        editUrl,
        editLocalizedFiles: true,
        sidebarPath: false,
      }),
    ],
    [
      "content-docs",
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: "use-cases",
        path: "use-cases",
        routeBasePath: "use-cases",
        editUrl,
        editLocalizedFiles: true,
      }),
    ],
    [
      "content-docs",
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: "topologies",
        path: "topologies",
        routeBasePath: "topologies",
        editUrl,
        editLocalizedFiles: true,
      }),
    ],
    [
      "content-docs",
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: "benchmarks",
        path: "benchmarks",
        routeBasePath: "benchmarks",
        editLocalizedFiles: true,
      }),
    ],
    [
      "@docusaurus/plugin-client-redirects",
      {
        redirects: [
          // Use cases section re-organized (2023-07-25)
          {
            from: "/use-cases/poker-game",
            to: "/use-cases/other/poker-game",
          },
          {
            from: "/use-cases/nft-auction",
            to: "/use-cases/auctions",
          },
          {
            from: "/use-cases/pay-per-use-api",
            to: "/use-cases/payments/pay-per-use-api",
          },
          {
            from: "/use-cases/inter-wallet-payments",
            to: "/use-cases/payments/inter-wallet-payments",
          },
        ],
      },
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      colorMode: {
        defaultMode: "light",
        disableSwitch: true,
        respectPrefersColorScheme: false,
      },
      navbar: {
        title: "Hydra Head Protocol",
        logo: {
          alt: "Hydra Head Protocol",
          src: "img/hydra.png",
          srcDark: "img/hydra-white.png",
        },
        items: [
          {
            to: "/user-manual",
            label: "User manual",
            position: "right",
          },
          {
            to: "/faq",
            label: "FAQ",
            position: "right",
          },
          {
            href: "https://github.com/cardano-scaling/hydra",
            html: `<svg
                    xmlns="http://www.w3.org/2000/svg"
                    width="25"
                    height="24"
                    viewBox="0 0 25 24"
                    fill="none"
                    {...props}
                  >
                    <path
                      fill-rule="evenodd"
                      clip-rule="evenodd"
                      d="M12.279 0C5.48905 0 0 5.49998 0 12.3042C0 17.7432 3.51702 22.3472 8.39607 23.9767C9.00607 24.0991 9.22952 23.7119 9.22952 23.3862C9.22952 23.1009 9.20941 22.1232 9.20941 21.1044C5.79368 21.8379 5.08238 19.6377 5.08238 19.6377C4.53345 18.2117 3.72011 17.8452 3.72011 17.8452C2.60214 17.0914 3.80154 17.0914 3.80154 17.0914C5.04166 17.1729 5.69239 18.3544 5.69239 18.3544C6.78999 20.2284 8.55869 19.6989 9.27023 19.3729C9.37178 18.5784 9.69726 18.0284 10.0429 17.7229C7.31857 17.4377 4.45227 16.3784 4.45227 11.6522C4.45227 10.3077 4.93987 9.20771 5.71249 8.35222C5.59059 8.04672 5.16356 6.78347 5.83464 5.09273C5.83464 5.09273 6.87143 4.76673 9.20916 6.35572C10.21 6.08639 11.2422 5.94938 12.279 5.94823C13.3158 5.94823 14.3727 6.09097 15.3487 6.35572C17.6867 4.76673 18.7234 5.09273 18.7234 5.09273C19.3945 6.78347 18.9672 8.04672 18.8453 8.35222C19.6383 9.20771 20.1058 10.3077 20.1058 11.6522C20.1058 16.3784 17.2395 17.4172 14.4949 17.7229C14.9423 18.1099 15.3283 18.8432 15.3283 20.0044C15.3283 21.6544 15.3082 22.9787 15.3082 23.3859C15.3082 23.7119 15.5319 24.0991 16.1417 23.9769C21.0207 22.3469 24.5377 17.7432 24.5377 12.3042C24.5578 5.49998 19.0487 0 12.279 0Z"
                      fill="currentColor"
                    />
                  </svg>`,
            position: "right",
          },
          {
            href: "https://discord.gg/Qq5vNTg9PT",
            html: `<svg
                    xmlns="http://www.w3.org/2000/svg"
                    width="23"
                    height="18"
                    viewBox="0 0 23 18"
                    fill="none"
                  >
                    <path
                      d="M19.4778 1.49632C18.0074 0.806713 16.4357 0.286253 14.7879 0C14.7626 0 14.7246 0 14.7119 0.0390345C14.5091 0.403356 14.2809 0.884782 14.1288 1.27513C12.3543 1.00189 10.6051 1.00189 8.8686 1.27513C8.71649 0.884782 8.47566 0.416368 8.27286 0.0390345C8.26019 0.0130115 8.22216 0 8.19681 0C6.54903 0.286253 4.9773 0.793701 3.51965 1.49632C3.50697 1.49632 3.4943 1.50933 3.4943 1.52234C0.50294 6.08938 -0.308275 10.5523 0.0973328 14.9632C0.0973328 14.9892 0.110008 15.0023 0.122683 15.0153C2.08734 16.4986 4.00131 17.3964 5.86457 17.9949C5.88992 17.9949 5.92794 17.9949 5.94062 17.9689C6.38425 17.3443 6.77718 16.6937 7.11941 16.0041C7.14476 15.9651 7.11941 15.9131 7.08139 15.9C6.4603 15.6528 5.86457 15.3536 5.2815 15.0283C5.2308 15.0023 5.2308 14.9372 5.2815 14.8982C5.40826 14.8071 5.52233 14.703 5.63641 14.6119C5.66176 14.5989 5.68711 14.5859 5.71246 14.6119C9.47701 16.3815 13.5584 16.3815 17.2723 14.6119C17.2976 14.6119 17.323 14.6119 17.3483 14.6119C17.4624 14.703 17.5892 14.8071 17.7032 14.8982C17.7413 14.9242 17.7413 15.0023 17.7032 15.0283C17.1328 15.3666 16.5371 15.6658 15.9034 15.9C15.8653 15.9131 15.84 15.9651 15.8653 16.0041C16.2076 16.6937 16.6005 17.3443 17.0441 17.9689C17.0568 17.9949 17.0948 18.0079 17.1202 17.9949C18.9961 17.3964 20.9101 16.4986 22.8747 15.0153C22.8874 15.0023 22.9001 14.9762 22.9001 14.9632C23.3817 9.86271 22.1015 5.4388 19.5031 1.52234C19.5031 1.50933 19.4904 1.49632 19.4778 1.49632ZM7.6898 12.2828C6.5617 12.2828 5.62374 11.2159 5.62374 9.90175C5.62374 8.58759 6.53635 7.52064 7.6898 7.52064C8.84325 7.52064 9.78121 8.6006 9.75586 9.90175C9.75586 11.2159 8.84325 12.2828 7.6898 12.2828ZM15.333 12.2828C14.2049 12.2828 13.2669 11.2159 13.2669 9.90175C13.2669 8.58759 14.1795 7.52064 15.333 7.52064C16.4864 7.52064 17.4244 8.6006 17.399 9.90175C17.399 11.2159 16.4864 12.2828 15.333 12.2828Z"
                      fill="currentColor"
                    />
                  </svg>`,
            position: "right",
          },
        ],
      },
      footer: {
        style: "dark",
        links: [
          {
            title: "Contributing",
            items: [
              {
                label: "Coding Standards",
                to: "https://github.com/cardano-scaling/hydra/wiki/Coding-Standards",
              },
              {
                label: "Architectural Decision",
                to: "/adr",
              },
              {
                label: "Testing Strategy",
                to: "https://github.com/cardano-scaling/hydra/wiki/Testing-Strategy",
              },
            ],
          },
          {
            title: "Community",
            items: [
              {
                label: "Discord",
                href: "https://discord.gg/Qq5vNTg9PT",
              },
              {
                label: "Github",
                href: "https://github.com/cardano-scaling/hydra/discussions",
              },
              {
                label: "Stack Exchange",
                href: "https://cardano.stackexchange.com/questions/tagged/hydra",
              },
            ],
          },
          {
            title: "More",
            items: [
              {
                label: "Haskell Packages",
                to: "/docs/dev/haskell-packages",
              },
              {
                label: "Monthly reports",
                to: "https://cardano-scaling.github.io/website/monthly",
              },
              {
                label: "Logbook",
                to: "https://github.com/cardano-scaling/hydra/wiki/Logbook",
              },
            ],
          },
          {
            title: "Legal",
            items: [
              {
                label: "Terms & Conditions",
                to: "https://static.iohk.io/terms/iohktermsandconditions.pdf",
              },
              {
                label: "Privacy Policy",
                to: "https://static.iohk.io/terms/iog-privacy-policy.pdf",
              },
              {
                label: "Contributors",
                to: "https://github.com/cardano-scaling/hydra/graphs/contributors",
              },
            ],
          },
        ],
        copyright: `© 2024`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        additionalLanguages: ["haskell"],
      },
      algolia: {
        appId: "YZTAF8IOVB",
        apiKey: "ad133fe3b0b40974c26853abc9cad2ab",
        indexName: "hydra-family",
        searchPagePath: "search",
        contextualSearch: true,
      },
    }),

  markdown: {
    mermaid: true,
  },

  themes: ["@docusaurus/theme-mermaid"],
};

module.exports = config;
