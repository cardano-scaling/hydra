// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require("prism-react-renderer/themes/github");
const darkCodeTheme = require("prism-react-renderer/themes/dracula");
const docsMetadataJson = require("./static/docs-metadata.json");

const customFields = {
  apiSpecDir: "../hydra-node/json-schemas",
  apiSpecUrl: "api.yaml",
};

const editUrl = "https://github.com/input-output-hk/hydra/tree/master/docs";

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Hydra: Head Protocol",
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

  i18n: {
    defaultLocale: "en",
    locales: ["en", "fr", "ja"],
  },

  presets: [
    [
      "classic",
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          editUrl,
          editLocalizedFiles: true,
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
          authorsMapPath: "authors.yml",
        },
        theme: {
          customCss: require.resolve("./src/css/custom.css"),
        },
      }),
    ],
  ],

  plugins: [
    [
      "content-blog",
      /** @type {import('@docusaurus/plugin-content-blog').Options} */
      ({
        id: "monthly",
        path: "monthly",
        routeBasePath: "monthly",
        authorsMapPath: "../authors.yaml",
        editUrl,
        editLocalizedFiles: true,
      }),
    ],
    [
      "content-docs",
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: "standalone",
        path: "standalone",
        routeBasePath: "/",
        editUrl,
        editLocalizedFiles: true,
      }),
    ],
    [
      "content-docs",
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: "core-concepts",
        path: "core-concepts",
        routeBasePath: "core-concepts",
        editUrl,
        editLocalizedFiles: true,
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
        editUrl,
        editLocalizedFiles: true,
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: "Hydra: Head Protocol",
        logo: {
          alt: "Hydra: Head Protocol",
          src: "img/hydra.png",
          srcDark: "img/hydra-white.png",
        },
        items: [
          {
            to: "/docs/getting-started",
            label: "User Manual",
            position: "left",
          },
          {
            to: "/use-cases",
            label: "Use Cases",
            position: "left",
          },
          {
            to: "/core-concepts",
            label: "Core Concepts",
            position: "left",
          },
          {
            to: "/topologies",
            label: "Topologies",
            position: "left",
          },
          {
            to: "/benchmarks",
            label: "Benchmarks",
            position: "left",
          },
          {
            to: "/api-reference",
            label: "API Reference",
            position: "left",
          },
          {
            href: "https://github.com/input-output-hk/hydra",
            label: "GitHub",
            position: "right",
          },
          {
            type: "localeDropdown",
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
                to: "https://github.com/input-output-hk/hydra/wiki/Coding-Standards",
              },
              {
                label: "Architectural Decision Records",
                to: "/adr",
              },
              {
                label: "Testing Strategy",
                to: "https://github.com/input-output-hk/hydra/wiki/Testing-Strategy",
              },
            ],
          },
          {
            title: "Community",
            items: [
              {
                label: "Discord (#ask-hydra)",
                href: "https://discord.gg/Qq5vNTg9PT",
              },
              {
                label: "Github Discussions",
                href: "https://github.com/input-output-hk/hydra/discussions",
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
                to: "/docs/haskell_packages",
              },
              {
                label: "Logbook",
                to: "https://github.com/input-output-hk/hydra/wiki/Logbook",
              },
              {
                label: "Input Output (Blog)",
                to: "https://iohk.io/en/blog",
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
                to: "https://github.com/input-output-hk/hydra/graphs/contributors",
              },
            ],
          },
        ],
        copyright: `
          <small>
          Built with Docusaurus on ${docsMetadataJson.site.lastUpdatedAt}
          </small>
          `,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
      },
      algolia: {
        appId: "YZTAF8IOVB",
        apiKey: "ad133fe3b0b40974c26853abc9cad2ab",
        indexName: "hydra-family",
        searchPagePath: "search",
        contextualSearch: true,
      },
    }),
};

module.exports = config;
