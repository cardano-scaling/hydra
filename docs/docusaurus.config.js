// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require("prism-react-renderer/themes/github");
const darkCodeTheme = require("prism-react-renderer/themes/dracula");
const docsMetadataJson = require("./static/docs-metadata.json");

const customFields = {
  apiSpecDir: "../hydra-node/json-schemas",
  apiSpecUrl: "api.yaml",
  docsearchAppId: "OF3CR7K89X",
  docsearchApiKey: "09b2fc0200d06fb433a5f4ced7c9d427",
};

const editUrl = "https://github.com/cardano-scaling/hydra/tree/master/docs";

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Hydra Head protocol documentation",
  url: "https://hydra.family",
  baseUrl: "/head-protocol/",
  // Note: This gives warnings about the haddocks; but actually they are
  // present. If you are concerned, please check the links manually!
  onBrokenLinks: "throw",
  onBrokenMarkdownLinks: "warn",
  favicon: "img/hydra.png",
  organizationName: "Input Output",
  projectName: "Hydra",
  staticDirectories: ["static", customFields.apiSpecDir],
  customFields,
  trailingSlash: false,

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
          blogTitle: "Architecture Decision Records",
          onUntruncatedBlogPosts: "ignore",
          blogDescription:
            "Lightweight technical documentation for the Hydra node software.",
          blogSidebarTitle: "Architecture Decision Records",
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
        disableSwitch: false,
        respectPrefersColorScheme: false,
      },
      navbar: {
        title: "Hydra Head protocol",
        logo: {
          alt: "Hydra Head protocol",
          src: "img/hydra.png",
          style: { height: 27, marginTop: 2.5 },
          srcDark: "img/hydra-white.png",
        },
        items: [
          {
            to: "/docs",
            label: "User manual",
            position: "left",
          },
          {
            to: "/docs/dev",
            label: "Developer documentation",
            position: "left",
          },
          {
            to: "/topologies",
            label: "Topologies",
            position: "right",
          },
          {
            to: "/use-cases",
            label: "Use cases",
            position: "right",
          },
          {
            to: "/docs/faqs",
            label: "FAQs",
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
                label: "Coding standards",
                to: "https://github.com/cardano-scaling/hydra/wiki/Coding-Standards",
              },
              {
                label: "Architecture Decision Records",
                to: "/adr",
              },
              {
                label: "Testing strategy",
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
                label: "GitHub discussions",
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
                label: "Haskell packages",
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
                label: "Terms and conditions",
                to: "https://static.iohk.io/terms/iog-terms-and-conditions.pdf",
              },
              {
                label: "Privacy policy",
                to: "https://static.iohk.io/terms/iog-privacy-policy.pdf",
              },
              {
                label: "Contributors",
                to: "https://github.com/cardano-scaling/hydra/graphs/contributors",
              },
            ],
          },
        ],
        copyright: `© 2025`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        additionalLanguages: ["haskell"],
      }
    }),

  markdown: {
    mermaid: true,
  },

  themes: ["@docusaurus/theme-mermaid"],
};

module.exports = config;
