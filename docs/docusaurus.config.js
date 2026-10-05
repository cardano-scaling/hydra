// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require("prism-react-renderer/themes/github");
const darkCodeTheme = require("prism-react-renderer/themes/dracula");
const docsMetadataJson = require("./static/docs-metadata.json");

const SITE_URL = "https://hydra.family";
const BASE_URL = "/head-protocol/";

// The version is set by the DOCS_VERSION environment variable during the build.
// It defaults to "unstable" for local development.
const VERSION = process.env.DOCS_VERSION || "unstable";

const isUnstable = VERSION === "unstable";

const customFields = {
  apiSpecDir: "../hydra-node/json-schemas",
  apiSpecUrl: "api.yaml",
  docsearchAppId: "OF3CR7K89X",
  docsearchApiKey: "09b2fc0200d06fb433a5f4ced7c9d427",
};

const communityLinks = [
  {
    label: "Github",
    href: "https://github.com/cardano-scaling/hydra",
  },
  {
    label: "Discord",
    href: "https://discord.gg/Qq5vNTg9PT",
  },
  {
    label: "Stack Exchange",
    href: "https://cardano.stackexchange.com/questions/tagged/hydra",
  },
  {
    label: "Monthly Reports",
    href: "https://cardano-scaling.github.io/website/monthly",
  },
  {
    label: "Protocol Specification",
    to: "/docs/dev/specification",
  },
];

const editUrl = "https://github.com/cardano-scaling/hydra/tree/master/docs";

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Hydra Head protocol documentation",
  url: SITE_URL,
  baseUrl: BASE_URL,
  // Note: This gives warnings about the haddocks; but actually they are
  // present. If you are concerned, please check the links manually!
  onBrokenLinks: "warn",
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
      announcementBar: isUnstable
        ? {
            id: "unstable_docs_banner",
            content: `This is the documentation for the unstable version of Hydra. For the latest stable version, see <a target="_blank" rel="noopener noreferrer" href="https://hydra.family/head-protocol/docs">here</a>.`,
            isCloseable: false,
          }
        : undefined,
      homepageNavbarItems: [
        {
          type: "html",
          position: "left",
          value: `<span class="navbar-version">${VERSION}</span>`,
        },
        {
          to: "/head-protocol/#why-hydra",
          label: "Why Hydra",
          position: "center",
        },
        {
          to: "/head-protocol/#how-it-works",
          label: "How it works",
          position: "center",
        },
        {
          to: "/head-protocol/#use-cases",
          label: "Use cases",
          position: "center",
        },
        {
          to: "/head-protocol/#topologies",
          label: "Topologies",
          position: "center",
        },
        {
          to: "/head-protocol/#developers",
          label: "Developers",
          position: "center",
        },
      ],
      navbar: {
        title: "Hydra",
        logo: {
          alt: "Hydra Head logo",
          src: "img/hydra.png",
          style: { height: 27, marginTop: 2.5 },
          srcDark: "img/hydra-white.png",
        },
        items: [
          {
            type: "html",
            position: "left",
            value: `<span class="navbar-version">${VERSION}</span>`,
          },
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
        logo: {
          alt: "Hydra",
          src: "img/hydra-with-text.png",
          width: 480,
          height: 135,
        },
        links: [
          ...communityLinks,
          {
            label: "Docs",
            to: "/docs",
          },
        ],
        copyright: `© Input Output Group, ${new Date().getFullYear()}.`,
      },
      footerDescription:
        "Hydra is open source, stewarded by Input Output Group (IOG), and funded in part by the Cardano treasury.",
      mobileSidebarLinks: communityLinks,
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        additionalLanguages: ["haskell"],
      },
    }),

  markdown: {
    mermaid: true,
  },

  themes: ["@docusaurus/theme-mermaid"],
};

module.exports = config;
