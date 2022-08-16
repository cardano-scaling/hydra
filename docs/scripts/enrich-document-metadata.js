#!/usr/bin/env node

const util = require('util')
const exec = util.promisify(require('child_process').exec)
const fs = require('fs')
const glob = require("glob")

const sysCall = async (cmd) => exec(cmd).then(dirs => dirs.stdout)

const Utils = {
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
        sysCall(`git --no-pager log -1 --pretty=format:'%ad' --date=local ${doc}`)
    , getRelativeTimeSince: async (doc) =>
        sysCall(`git --no-pager log -1 --pretty=format:'%cr' ${doc}`)
    , getCommitHash: async (doc) =>
        sysCall(`git --no-pager log -1 --pretty=format:'%h' ${doc}`)
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
    , getDocumentMetadata: async (doc) => {
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
}

async function main() {

    const docs = await Utils.getItems("**/*.md", {
        "ignore": ['**/node_modules/**']
    })

    let metadatas = {}

    for (const doc of docs) {
        const metadata = await Utils.getDocumentMetadata(doc)
        metadatas = { ...metadatas, ...metadata }
    }

    const metadatasPath = "static/metadatas.json"
    fs.writeFile(
        metadatasPath,
        JSON.stringify(metadatas, null, 2),
        err => { if (err) return console.log(err) }
    )
}

main()
