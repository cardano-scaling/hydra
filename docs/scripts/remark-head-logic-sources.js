// Keep the committed Markdown's repository-relative links useful on GitHub,
// while linking the website to the source revision used for its build.
module.exports = function headLogicSources({ revision }) {
  const prefix = "../../../../hydra-node/src/";
  const base = `https://github.com/cardano-scaling/hydra/blob/${revision}/hydra-node/src/`;
  return function transform(tree) {
    function visit(node) {
      if (node.type === "link" && node.url.startsWith(prefix)) {
        node.url = base + node.url.slice(prefix.length);
      }
      if (node.children) node.children.forEach(visit);
    }
    visit(tree);
  };
};
