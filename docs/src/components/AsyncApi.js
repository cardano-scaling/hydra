import React, { useEffect } from "react";
import Layout from "@theme/Layout";
import { useLocation } from "@docusaurus/router";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import {
  useVersions,
  useActiveDocContext,
} from "@docusaurus/plugin-content-docs/client";

// A react component to render the AsyncAPI associated with the currently loaded
// documentation version.
//
// NOTE: This is really a sledge-hammer approach to get async api rendered in
// docusaurus. Other approaches via webpack and more idiomatic inclusion of
// react components did not work well.
//
// XXX: Seems like this should be done via a plugin in the first place like it
// is done by https://github.com/rohit-gohri/redocusaurus for OpenAPI specs
export default function AsyncApi() {
  const { siteConfig } = useDocusaurusContext();
  const activeDocContext = useActiveDocContext();

  const docsPath = activeDocContext.activeVersion.path;
  const apiSpec =
    docsPath +
    (docsPath.endsWith("/") ? "" : "/") +
    siteConfig.customFields.apiSpecUrl;
  console.log("Loading api spec: ", apiSpec);

  useEffect(() => {
    const lib = document.createElement("script");
    lib.src =
      "https://unpkg.com/@asyncapi/react-component@1.0.0-next.47/browser/standalone/index.js";

    const scriptTag = document.createElement("script");
    scriptTag.innerHTML = `(() => {
      AsyncApiStandalone.render({
        schema: {
          url: '${apiSpec}',
        },
        config: {
          show: {
            sidebar: true,
            messages: false,
          },
        },
      }, document.getElementById('asyncapi'));
    })();`;

    lib.addEventListener("load", () => {
      document.body.appendChild(scriptTag);
      setTimeout(() => {
        document.querySelector("div.loader").style.display = "none";
      }, 250);
    });
    document.body.appendChild(lib);

    return () => {
      document.body.removeChild(lib);
      document.body.removeChild(scriptTag);
    };
  }, []);

  return (
    <main>
      <div id="asyncapi"></div>
      <link
        rel="stylesheet"
        href="https://unpkg.com/@asyncapi/react-component@1.0.0-next.47/styles/default.min.css"
      />
      <style>
        {overrideDocusaurusPageBounds()}
        {darkThemeSupport()}
      </style>
      <div className="loader"></div>
    </main>
  );
}

function overrideDocusaurusPageBounds() {
  // Use full width of non-navigation space
  return Array.prototype.concat([
    `
    #__docusaurus .container,
    #__docusaurus .container .col,
    #__docusaurus .container .col article {
      max-width: inherit !important;
    }
  `,
    // Add some space to bottom of sidebar to overflow less with footer and remove shadow
    `
    #asyncapi .sidebar--content {
      margin-bottom: 500px;
    }
    #asyncapi .aui-root .shadow {
      --tw-shadow: inherit;
    }
  `,
  ]);
}

// Strawman dark-theme support for AsyncApi
function darkThemeSupport() {
  return `
    .z-10 {
      z-index: 1 !important;
    }

    .z-20 {
      z-index: 2 !important;
    }

    div.sidebar--content > div > h1 {
      display: none;
    }

    a[href^='#messages'], a[href^='#messages']+* {
      display: none !important;
    }

    .bg-white {
      background: var(--ifm-background-color) !important;
    }

    .bg-gray-100 {
      background: var(--ifm-code-background) !important;
    }

    .bg-gray-200 {
      background: var(--ifm-card-background-color) !important;
    }

    .prose {
      color: var(--ifm-font-color-base) !important;
    }

    .prose blockquote {
      color: var(--ifm-font-color-secondary) !important;
    }

    .prose code {
      color: var(--ifm-color-primary) !important;
    }

    svg.cursor-pointer {
      fill: var(--ifm-font-color-base) !important;
    }

    .examples svg.cursor-pointer {
      fill: var(--ifm-color-white) !important;
    }

    .text-gray-600, .text-gray-700 {
      color: var(--ifm-font-color-secondary) !important;
    }
  `;
}
