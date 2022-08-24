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
    , docPathToKey: (docPath) => {
        const basePath = docPath.replace('.md', '')
            .replace('i18n/', '')
            .replace('/current/', '/')
            .replace('/index', '')

        // @TODO rename to docusaurus-plugin-content-docs-docs
        const documentPath = basePath.includes('docusaurus-plugin-content-docs-') ?
            basePath.replace('docusaurus-plugin-content-docs-', '') :
            basePath.replace('docusaurus-plugin-content-', '')

        if (documentPath.includes('adr'))
            return 'adr/' + parseInt(documentPath.split('_')[1].split('-')[0]).toString()
        return documentPath
    }
    , getSupportedLanguages: async () => {
        const dirs = await Utils.sysCall('ls i18n/')
        return dirs.trim().split('\n')
    }
    , getDocsMetadataJson: async (docPath) => {
        console.log("Processing: ", docPath)

        const lastUpdatedAt = await Utils.getLastUpdatedAt(docPath)
        const commitHash = await Utils.getCommitHash(docPath)
        const supportedLanguages = await Utils.getSupportedLanguages()

        const docKey = Utils.docPathToKey(docPath)
        const [head, ...tail] = docKey.split("/")
        const isSupportedLanguage = (language) => supportedLanguages.includes(language)
        const key = isSupportedLanguage(head) ? tail.join('/') : docKey
        const subKey = isSupportedLanguage(head) ? head : "source"

        return {
            [key]: {
                lastUpdatedAt,
                commitHash,
                subKey
            }
        }
    }
    , mergeDocsMetadataJson: (jsons) =>
        jsons.reduce((obj, item) => {
            for (const key in item) {
                if (obj[key] === undefined) {
                    obj[key] = {}
                }
                const { lastUpdatedAt, commitHash, subKey } = item[key]
                obj[key][subKey] = { lastUpdatedAt, commitHash }
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

    Utils.getItems("**/*.md", { "ignore": ['**/node_modules/**', 'README.md'] })
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
