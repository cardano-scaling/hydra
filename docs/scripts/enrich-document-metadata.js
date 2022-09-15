#!/usr/bin/env node

const util = require('util')
const exec = util.promisify(require('child_process').exec)
const fs = require('fs')
const glob = require("glob")

const Utils = {
    sysCall: async (cmd) => exec(cmd).then(dirs => dirs.stdout),
    getItems: async (path, options) => {
        return new Promise((resolve, reject) => {
            glob(path, options, (err, res) => {
                if (err) {
                    reject(err)
                }
                resolve(res)
            })
        })
    }
    , getLastUpdatedAt: async (doc) =>
        Utils.sysCall(`git --no-pager log -1 --pretty=format:'%aI' ${doc}`)
    , getCommitHash: async (doc) =>
        Utils.sysCall(`git --no-pager log -1 --pretty=format:'%H' ${doc}`)
    , getDocsMetadataJson: async (docPath) => {
        console.log("Processing: ", docPath)

        const lastUpdatedAt = await Utils.getLastUpdatedAt(docPath)
        const commitHash = await Utils.getCommitHash(docPath)

        let lang = "source"
        let docKey = docPath
        const parts = docPath.split("/")
        if (parts[0] == "i18n") {
            lang = parts[1]
            docKey = parts[2] == 'docusaurus-plugin-content-docs'
                ? 'docs/'
                : parts[2].replace('docusaurus-plugin-content-', '')
            // parts[3] is the version - we don't need it
            docKey = docKey + parts.slice(4).join("/")
        }
        docKey = docKey.replace('.md', '').replace('/index', '')

        // NOTE: This assumes the file name include the adr number and that this
        // is consistent with the slug used for serving the page later
        if (docKey.includes('adr')) {
            docKey = 'adr/' + parseInt(docKey.split('_')[1].split('-')[0]).toString()
        }

        return {
            [docKey]: {
                lastUpdatedAt,
                commitHash,
                lang
            }
        }
    }
    , mergeDocsMetadataJson: (jsons) =>
        jsons.reduce((obj, item) => {
            for (const key in item) {
                if (obj[key] === undefined) {
                    obj[key] = {}
                }
                const { lastUpdatedAt, commitHash, lang } = item[key]
                obj[key][lang] = { lastUpdatedAt, commitHash }
            }
            return obj
        }, {})
    , writeJsonToFile: (file) => (json) => {
        fs.writeFileSync(file, JSON.stringify(json, null, 2))
    },
    enrichWithSiteMetadata: async (json) => {
        console.log("Processing site metadata")
        const lastUpdatedAt = await Utils.getLastUpdatedAt('docs')
        return {
            "site": {
                lastUpdatedAt
            },
            ...json
        };
    }
}

async function main() {

    Utils.getItems("**/*.md", {
        "ignore": [
            '**/node_modules/**',
            'README.md',
            'docs/glossary.md'
        ]
    })
        .then(docs => {
            const docsMetadata = docs.map(Utils.getDocsMetadataJson)
            Promise
                .all(docsMetadata)
                .then(Utils.mergeDocsMetadataJson)
                .then(Utils.enrichWithSiteMetadata)
                .then(Utils.writeJsonToFile("static/docs-metadata.json"))
        })
}

main()
