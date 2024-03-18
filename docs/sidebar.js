module.exports = {
  userDocumentation: [
    {
      type: "doc",
      label: "Welcome",
      id: "getting-started/index",
    },
    {
      type: "html",
      value: "<small><b>Tutorials</b></small>",
      defaultStyle: true,
    },
    {
      type: "link",
      href: "/",
      label: "Quick Start",
    },
    {
      type: "doc",
      id: "tutorial/index",
      label: "Open a head on testnet",
    },
    {
      type: "html",
      value: "<small><b>Core Concepts</b></small>",
      defaultStyle: true,
    },
    {
      type: "link",
      href: "/",
      label: "Protocol overview",
    },
    "known-issues",
    "faq",
    {
      type: "html",
      value: "<small><b>Documentation</b></small>",
      defaultStyle: true,
    },
    {
      type: "link",
      href: "/",
      label: "Configuration",
    },
    {
      type: "link",
      href: "/",
      label: "Reference scripts",
    },
    {
      type: "link",
      href: "/",
      label: "Head parameters",
    },
    {
      type: "link",
      href: "/",
      label: "Commit funds",
    },
    {
      type: "category",
      label: "How to ...",
      collapsed: true,
      collapsible: true,
      items: [
        {
          type: "link",
          href: "/",
          label: "Run Hydra on mainnet",
        },
        {
          type: "link",
          href: "/",
          label: "Use Hydra in a DApp",
        },
        {
          type: "link",
          href: "/",
          label: "Commit a script to a head",
        },
      ],
    },
    {
      type: "html",
      value: "<small><b>Reference</b></small>",
      defaultStyle: true,
    },
    {
      type: "link",
      href: "https://github.com/input-output-hk/hydra/releases",
      label: "Release notes",
    },
    {
      type: "link",
      href: "/api-reference",
      label: "API reference",
    },
    {
      type: "link",
      href: "/benchmarks",
      label: "Benchmarks",
    },
  ],
};
