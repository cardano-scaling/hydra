#!/usr/bin/env node

const util = require('util');
const exec = util.promisify(require('child_process').exec);
const fs = require('fs');

const sysCall = async (cmd) => exec(cmd).then(dirs => dirs.stdout)

const Utils = {
    getDirectories: async (docsDir) =>
        sysCall(`git ls-files ${docsDir}`)
            .then(dirs => dirs.trim()
                .split('\n'))
    , getLastUpdatedAt: async (doc) =>
        sysCall(`git --no-pager log -1 --pretty=format:'%ad' --date=local ${doc}`)
    , getRelativeTimeSince: async (doc) =>
        sysCall(`git --no-pager log -1 --pretty=format:'%cr' ${doc}`)
    , getCommitHash: async (doc) =>
        sysCall(`git --no-pager log -1 --pretty=format:'%h' ${doc}`)
    , getLastTranslatedAt: async () =>
        sysCall(`git --no-pager log -1 --pretty=format:'%ad' --date=local i18n`)
    , getDocumentMetadata: async (doc) => {
        const lastUpdatedAt = await Utils.getLastUpdatedAt(doc)
        const relativeTimeSince = await Utils.getRelativeTimeSince(doc)
        const commitHash = await Utils.getCommitHash(doc)
        return {
            [doc.replace('.md', '')]: {
                lastUpdatedAt,
                relativeTimeSince,
                commitHash
            }
        }
    }
}

async function main() {
    const docsRegex = "docs/*.md"
    const docs = await Utils.getDirectories(docsRegex);

    let metadatas = {}

    for (const doc of docs) {
        const metadata = await Utils.getDocumentMetadata(doc)
        metadatas = { ...metadatas, ...metadata }
    }

    const lastTranslatedAt = await Utils.getLastTranslatedAt()
    metadatas = { ...metadatas, ...{ lastTranslatedAt } }

    const metadatasPath = "static/metadatas.json"
    fs.writeFile(
        metadatasPath,
        JSON.stringify(metadatas, null, 2),
        err => { if (err) return console.log(err) }
    )
}

main();
