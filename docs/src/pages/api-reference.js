import React from "react";
import Layout from "@theme/Layout";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";

export default function Home() {
  const { siteConfig } = useDocusaurusContext();
  // NOTE: I have *really* tried. But Webpack, react, babel,
  // preloaders, polyfills and all are just a pain in the
  // *** to work with. Nothing makes sense, installing AND
  // NOT USING dependencies has effect on the build outcome
  // and so do completely unrelated plugins. Frontend JS, fix
  // yourself.
  //
  // I've lost already 3h on that, so here is the sledge-hammer.
  const asyncApi = `
    <div id="asyncapi"></div>
    <link rel="stylesheet" href="https://unpkg.com/@asyncapi/react-component@1.0.0-next.34/styles/default.min.css">
    <style>
      .z-10 {
        z-index: 2 !important;
      }
    </style>
    <script src="https://unpkg.com/@asyncapi/react-component@1.0.0-next.34/browser/standalone/index.js"></script>
    <script>
      AsyncApiStandalone.render({
        schema: {
          url: '${siteConfig.baseUrl}${siteConfig.customFields.apiSpecUrl}',
        },
      }, document.getElementById('asyncapi'));
    </script>
  `;
  return (
    <Layout title={`${siteConfig.title}`}>
      <main dangerouslySetInnerHTML={{__html: asyncApi}}></main>
    </Layout>
  );
}
