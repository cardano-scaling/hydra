#!/usr/bin/env node

const AsyncApiValidator = require('asyncapi-validator');
const fs = require('fs');
const path = require('path');
const assert = require('assert');
const { customFields } = require('./docusaurus.config.js')

async function main() {
  const specFile = path.join(customFields.apiSpecDir, customFields.apiSpecUrl);
  const validator = await AsyncApiValidator.fromSource(specFile, { msgIdentifier: 'title', path: specFile });
  const [_node, _script, operation, channel, golden, stopAtFirst] = process.argv;
  const examples = JSON.parse(fs.readFileSync(golden));

  let errors = 0;
  examples.samples.forEach(x => {
    try {
      validator.validate(x.tag, x, channel, operation);
      console.log("✓", x.tag, "→", ellipse(60, JSON.stringify(x)));
    } catch (e) {
      errors += 1;
      console.log("🕱", x.tag, "→", e.message);
      console.log(indent(4, JSON.stringify(x, null, 4)));
      if (stopAtFirst) {
        process.exit(1);
      }
    }
  });

  assert(errors == 0, `Found ${errors} API validation error(s). Please fix.`)
}

function ellipse(n, str) {
  return str.substr(0, n-1) + (str.length > n ? "..." : "");
}

function indent(n, str) {
  return str.split('\n').map(x => ' '.repeat(n) + x).join('\n');
}

main();
