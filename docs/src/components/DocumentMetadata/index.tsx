import React, { useEffect, useState } from 'react'
import styles from './styles.module.css'
import metadatas from '@site/static/metadatas.json'

interface Props { }

interface Metadata {
  lastUpdatedAt: string
  relativeTimeSince: string
  commitHash: string
}

interface TimeObject {
  unit: string
  value: number
}

const Utils = {
  getRelativeMillisTimeSince: (lastUpdatedAt: string, lastTranslatedAt: string) => {
    const docLastUpdatedAt = new Date(lastUpdatedAt).getTime()
    const docLastTranslatedAt = new Date(lastTranslatedAt).getTime()
    const relativeTimeSince = docLastTranslatedAt - docLastUpdatedAt
    return relativeTimeSince
  }
  , getRelativeTimeSince: (relativeTime: number): TimeObject => {
    const units = ['seconds', 'minutes', 'hours', 'days']
    const divisors = [1000, 1000 * 60, 1000 * 60 * 60, 1000 * 60 * 60 * 24]
    const zip = (a: any, b: any) => a.map((k: any, i: any) => [k, b[i]])

    const relativeTimes = zip(units, divisors)
      .map((pair: any) => {
        const [unit, divisor] = pair
        const value = Math.floor(relativeTime / divisor)
        return { value, unit }
      })

    return relativeTimes.reduce((acc: TimeObject, obj: TimeObject) => {
      return (obj.value >= 0 && obj.value <= acc.value) ? obj : acc
    }, relativeTimes[0])
  }
  , getTimeObject: (defaultMetadata: Metadata, lastTranslatedAt: string): TimeObject => {
    const relativeMillisTimeSince =
      Utils.getRelativeMillisTimeSince(defaultMetadata.lastUpdatedAt, lastTranslatedAt)
    const relativeTimeSince =
      Utils.getRelativeTimeSince(relativeMillisTimeSince)
    return relativeTimeSince;
  }
}

const Display = {
  renderLastUpdatedAt: (lastUpdatedAt: string, relativeTimeSince: string) =>
    <i className={styles.info}>
      Last updated at: <b>{lastUpdatedAt}</b>
      (<b>{relativeTimeSince}</b> since last change)
    </i>
  , renderCommitHash: (commitHash: string, link: string) =>
    <i className={styles.info}>
      Last commit hash: <a href={link}><b>{commitHash}</b></a>
    </i>
  , renderLastTranslatedAt: (documentPath: string, lastTranslatedAt: string) => {
    const languages = ['fr', 'ja'] //@TODO move to config
    const [language, ...englishPath] = documentPath.split("/")

    if (!languages.includes(language)) {
      return <div className={styles.block}></div>
    }

    const defaultMetadata: Metadata = metadatas[englishPath.join("/")]

    if (defaultMetadata === undefined) {
      return <div className={styles.block}></div>
    }

    const timeObject = Utils.getTimeObject(defaultMetadata, lastTranslatedAt)

    if (timeObject.value === 0) {
      return <div className={styles.block}></div>
    }

    return <i className={styles.info}>
      Last translated at: <b>{lastTranslatedAt}</b>
      (⚠️ Warning: <b>{timeObject.value}</b> {timeObject.unit} behind default language)
    </i>
  }
}

export default function DocumentMetadata({ }: Props): JSX.Element {
  const [documentPath, setDocumentPath] = useState('placeholder')

  useEffect(() => {
    if (window) {
      const path = new URL(window.location.href).pathname.replace('/head-protocol/', '')
      setDocumentPath(path)
    }
  }, [])

  if (documentPath === 'placeholder') {
    return <div className={styles.block}></div>
  }

  const metadata: Metadata = metadatas[documentPath]

  if (metadata === undefined) {
    return <div className={styles.block}></div>
  }

  const { lastUpdatedAt, relativeTimeSince, commitHash } = metadata
  const link = `https://github.com/input-output-hk/hydra-poc/commit/${metadata.commitHash}`

  return <div className={styles.block}>
    {
      Display.renderLastUpdatedAt(
        lastUpdatedAt,
        relativeTimeSince
      )
    }
    {
      Display.renderCommitHash(commitHash, link)
    }
    {
      Display.renderLastTranslatedAt(documentPath, lastUpdatedAt)
    }
  </div >

}
