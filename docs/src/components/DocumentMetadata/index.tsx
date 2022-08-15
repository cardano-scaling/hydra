import React from 'react'
import styles from './styles.module.css'
import metadatas from '@site/static/metadatas.json'

interface Props { }

interface Metadata {
  lastUpdatedAt: string
  relativeTimeSince: string
  commitHash: string
}

export default function DocumentMetadata({ }: Props): JSX.Element {
  const path = new URL(window.location.href).pathname.replace('/head-protocol/', '')
  const metadata: Metadata = metadatas[path]
  const lastTranslatedAt: string | undefined = metadatas['lastTranslatedAt']

  const link =
    `https://github.com/input-output-hk/hydra-poc/commit/${metadata?.commitHash}`

  const renderLastUpdatedAt = (lastUpdatedAt: string, relativeTimeSince: string) =>
    <i className={styles.info}>
      Last updated at: <b>{lastUpdatedAt}</b> (
      <b>{relativeTimeSince}</b> since last change
      )
    </i>

  const renderCommitHash = (commitHash: string) =>
    <i className={styles.info}>Last commit hash: <a href={link}><b>{commitHash}</b></a></i>

  const renderLastTranslatedAt = (lastTranslatedAt: string) =>
    <i className={styles.info}>Last translated at: <b>{lastTranslatedAt}</b></i>

  const { lastUpdatedAt, relativeTimeSince, commitHash } = metadata

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
        renderCommitHash(commitHash)
      }
      {lastTranslatedAt && hasMetadata &&
        renderLastTranslatedAt(lastTranslatedAt)
      }
    </div >
  )
}
