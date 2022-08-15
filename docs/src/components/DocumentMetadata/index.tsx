import React, { useEffect, useState } from 'react'
import styles from './styles.module.css'
import metadatas from '@site/static/metadatas.json'

interface Props { }

interface Metadata {
  lastUpdatedAt: string
  relativeTimeSince: string
  commitHash: string
}

export default function DocumentMetadata({ }: Props): JSX.Element {
  const [documentPath, setDocumentPath] = useState('placeholder')

  useEffect(() => {
    if (window) {
      const path = new URL(window.location.href).pathname.replace('/head-protocol/', '')
      setDocumentPath(path)
    }
  }, [])

  const renderLastUpdatedAt = (lastUpdatedAt: string, relativeTimeSince: string) =>
    <i className={styles.info}>
      Last updated at: <b>{lastUpdatedAt}</b>
      (<b>{relativeTimeSince}</b> since last change)
    </i>

  const renderCommitHash = (commitHash: string, link: string) =>
    <i className={styles.info}>
      Last commit hash: <a href={link}><b>{commitHash}</b></a>
    </i>

  const getRelativeMillisTimeSince = (lastUpdatedAt: string, lastTranslatedAt: string) => {
    const docLastUpdatedAt = new Date(lastUpdatedAt).getTime()
    const docLastTranslatedAt = new Date(lastTranslatedAt).getTime()
    const relativeTimeSince = docLastTranslatedAt - docLastUpdatedAt
    return relativeTimeSince
  }

  interface TimeObject {
    unit: string
    value: number
  }
  const getRelativeTimeSince = (relativeTime: number): TimeObject => {
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

  const renderLastTranslatedAt = (documentPath: string, lastTranslatedAt: string) => {
    const languages = ['fr', 'ja'] //@TODO move to config
    const [language, ...englishPath] = documentPath.split("/")

    if (languages.includes(language)) {
      const defaultMetadata: Metadata = metadatas[englishPath.join("/")]
      const hasDefaultMetadata = !(defaultMetadata === undefined)

      if (hasDefaultMetadata) {
        const relativeMillisTimeSince =
          getRelativeMillisTimeSince(defaultMetadata.lastUpdatedAt, lastTranslatedAt)

        if (relativeMillisTimeSince > 0) {
          const relativeTimeSince =
            getRelativeTimeSince(relativeMillisTimeSince)

          return <i className={styles.info}>
            Last translated at: <b>{lastTranslatedAt}</b>
            (⚠️ Warning: <b>{relativeTimeSince.value}</b> {relativeTimeSince.unit} behind default language)
          </i>
        }
      }
      return <div className={styles.block}></div>
    }
  }

  if (!(documentPath === 'placeholder')) {
    const metadata: Metadata = metadatas[documentPath]
    const { lastUpdatedAt, relativeTimeSince, commitHash } = metadata
    const link = `https://github.com/input-output-hk/hydra-poc/commit/${metadata.commitHash}`

    const hasMetadata = !(metadata === undefined)
    return (
      <div className={styles.block}>
        {hasMetadata &&
          renderLastUpdatedAt(
            lastUpdatedAt,
            relativeTimeSince
          )
        }
        {hasMetadata &&
          renderCommitHash(commitHash, link)
        }
        {hasMetadata &&
          renderLastTranslatedAt(documentPath, lastUpdatedAt)
        }
      </div >
    )
  } else {
    return <div className={styles.block}></div>
  }
}
