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
        Utils.sysCall(`git --no-pager log -1 --pretty=format:'%ad' --date=local ${doc}`)
    , getRelativeTimeSince: async (doc) =>
        Utils.sysCall(`git --no-pager log -1 --pretty=format:'%cr' ${doc}`)
    , getCommitHash: async (doc) =>
        Utils.sysCall(`git --no-pager log -1 --pretty=format:'%h' ${doc}`)
    , pathToLocation: (path) => {
        const basePath = path.replace('.md', '')
            .replace('i18n/', '')
            .replace('/current/', '/')
            .replace('/index', '')

        // @TODO rename to docusaurus-plugin-content-docs-docs
        if (path.includes('docusaurus-plugin-content-docs-')) {
            return basePath.replace('docusaurus-plugin-content-docs-', '')
        } else {
            return basePath.replace('docusaurus-plugin-content-', '')
        }
    }
    , getMetadatasJson: async (doc) => {
        console.log("Processing: ", doc)
        const lastUpdatedAt = await Utils.getLastUpdatedAt(doc)
        const relativeTimeSince = await Utils.getRelativeTimeSince(doc)
        const commitHash = await Utils.getCommitHash(doc)
        const docKey = Utils.pathToLocation(doc)
        return {
            [docKey]: {
                lastUpdatedAt,
                relativeTimeSince,
                commitHash
            }
        }
    }
    , mergeJsons: (jsons) =>
        jsons.reduce((obj, item) => {
            for (const key in item) {
                obj[key] = item[key]
            }
            return obj
        }, {})
    , writeJsonToFile: (file) => (json) => {
        fs.writeFileSync(file, JSON.stringify(json, null, 2))
    }
}

async function main() {

    Utils.getItems("**/*.md", { "ignore": ['**/node_modules/**'] })
        .then(docs => {
            const metadatas = docs.map(Utils.getMetadatasJson)
            Promise
                .all(metadatas)
                .then(Utils.mergeJsons)
                .then(Utils.writeJsonToFile("static/docs-metadata.json"))
        })
}

main()
