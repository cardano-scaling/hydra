// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

const customFields = {
  apiSpecDir: '../hydra-node/json-schemas',
  apiSpecUrl: 'api.yaml',
};

const editUrl = 'https://github.com/input-output-hk/hydra-poc/tree/master/docs';

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Hydra: Head Protocol',
  url: 'https://input-output-hk.github.io',
  baseUrl: '/head-protocol/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/hydra.png',
  organizationName: 'Input Output',
  projectName: 'Hydra',
  staticDirectories: ['static', customFields.apiSpecDir],
  customFields,

  scripts: [
    {
      src: 'https://plausible.io/js/script.js',
      defer: true,
      'data-domain': 'hydra.family'
    }
  ],

  i18n: {
    defaultLocale: 'en',
    locales: ['en', 'fr'],
  },

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: { editUrl },
        blog: {
          path: 'adr',
          routeBasePath: '/adr',
          blogTitle: 'Architectural Decision Records',
          blogDescription: 'Lightweight technical documentation for the Hydra node software.',
          blogSidebarTitle: 'Architectural Decision Records',
          blogSidebarCount: 'ALL',
          sortPosts: 'ascending',
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  plugins: [
    [
      'content-docs',
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: 'core-concepts',
        path: 'core-concepts',
        routeBasePath: 'core-concepts',
        editUrl
      })
    ],
    [
      'content-docs',
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: 'use-cases',
        path: 'use-cases',
        routeBasePath: 'use-cases',
        editUrl
      })
    ],
    [
      'content-docs',
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: 'topologies',
        path: 'topologies',
        routeBasePath: 'topologies',
        editUrl
      }),
    ],
    [
      'content-docs',
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: 'benchmarks',
        path: 'benchmarks',
        routeBasePath: 'benchmarks',
        editUrl
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: 'Hydra: Head Protocol',
        logo: {
          alt: 'Hydra: Head Protocol',
          src: 'img/hydra.png',
          srcDark: 'img/hydra-white.png',
        },
        items: [
          {
            to: '/docs/getting-started',
            label: 'User Manual',
            position: 'left',
          },
          {
            to: '/use-cases',
            label: 'Use Cases',
            position: 'left',
          },
          {
            to: '/core-concepts',
            label: 'Core Concepts',
            position: 'left',
          },
          {
            to: '/topologies',
            label: 'Topologies',
            position: 'left',
          },
          {
            to: '/benchmarks',
            label: 'Benchmarks',
            position: 'left',
          },
          {
            to: '/api-reference',
            label: 'API Reference',
            position: 'left',
          },
          {
            href: 'https://github.com/input-output-hk/hydra-poc',
            label: 'GitHub',
            position: 'right',
          },
          {
            type: 'localeDropdown',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Contributing',
            items: [
              {
                label: 'Coding Standards',
                to: 'https://github.com/input-output-hk/hydra-poc/wiki/Coding-Standards',
              },
              {
                label: 'Architectural Decision Records',
                to: '/adr',
              },
              {
                label: 'Testing Strategy',
                to: 'https://github.com/input-output-hk/hydra-poc/wiki/Testing-Strategy',
              }
            ],
          },
          {
            title: 'Community',
            items: [
              {
                label: 'Discord (#ask-hydra)',
                href: 'https://discord.gg/Qq5vNTg9PT',
              },
              {
                label: 'Github Discussions',
                href: 'https://github.com/input-output-hk/hydra-poc/discussions',
              },
              {
                label: 'Stack Exchange',
                href: 'https://cardano.stackexchange.com/questions/tagged/hydra',
              },
            ],
          },
          {
            title: 'More',
            items: [
              {
                label: 'Haskell Packages',
                to: '/docs/haskell_packages'
              },
              {
                label: 'Logbook',
                to: 'https://github.com/input-output-hk/hydra-poc/wiki/Logbook'
              },
              {
                label: 'Input Output (Blog)',
                to: 'https://iohk.io/en/blog'
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} <strong>Input Output</strong> <br/> <small>Built with Docusaurus</small>`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
      },
      algolia: {
        appId: 'YZTAF8IOVB',
        apiKey: 'ad133fe3b0b40974c26853abc9cad2ab',
        indexName: 'hydra-family',
        searchPagePath: 'search',
        contextualSearch: true,
      }
    }),
};

module.exports = config;
