// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Hydra: Head Protocol',
  tagline: 'User Manual, core concepts and API reference.',
  url: 'https://input-output-hk.github.io',
  baseUrl: '/hydra-poc/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/hydra.png',
  organizationName: 'Input Output',
  projectName: 'Hydra',

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          editUrl: 'https://github.com/input-output-hk/hydra-poc/tree/master/docs'
        },
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
            to: '/docs/intro',
            label: 'User Manual',
            position: 'left',
          },
          {
            to: '#TODO-core-concepts',
            label: 'Core Concepts',
            position: 'left',
          },
          {
            to: '#TODO-api-reference',
            label: 'API Reference',
            position: 'left',
          },
          {
            href: 'https://github.com/input-output-hk/hydra-poc',
            label: 'GitHub',
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
    }),
};

module.exports = config;
