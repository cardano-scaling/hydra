import React, { useEffect } from "react";
import Layout from "@theme/Layout";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";

// NOTE: I have *really* tried. But Webpack, react, babel,
// preloaders, polyfills and all are just a pain in the
// *** to work with. Nothing makes sense, installing AND
// NOT USING dependencies has effect on the build outcome
// and so do completely unrelated plugins. Frontend JS, fix
// yourself.
//
// I've lost already 3h on that, so here is the sledge-hammer.
export default function AsyncApi() {
  const { siteConfig } = useDocusaurusContext();

  useEffect(() => {
      const scriptTag = document.createElement('script');
      scriptTag.innerHTML = `(() => {
        AsyncApiStandalone.render({
          schema: {
            url: '${siteConfig.baseUrl}${siteConfig.customFields.apiSpecUrl}',
          },
        }, document.getElementById('asyncapi'));
      })();`;
      document.body.appendChild(scriptTag);

      return () => { document.body.removeChild(scriptTag); }
  }, []);

  return (
    <Layout title={`${siteConfig.title}`}>
      <main>
        <div id="asyncapi"></div>
        <link rel="stylesheet" href="https://unpkg.com/@asyncapi/react-component@1.0.0-next.34/styles/default.min.css" />
        <style>{darkThemeSupport()}</style>
        <script src="https://unpkg.com/@asyncapi/react-component@1.0.0-next.34/browser/standalone/index.js"></script>
      </main>
    </Layout>
  );
}

// Strawman dark-theme support for AsyncApi
function darkThemeSupport() {
  return `
    .z-10 {
      z-index: 2 !important;
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

    .bg-gray-800 {
      background: var(--ifm-color-primary-dark) !important;
    }

    .prose {
      color: var(--ifm-font-color-base) !important;
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

    .examples .text-gray-600 {
      color: var(--ifm-color-primary-contrast-background) !important;
    }
  `
}
