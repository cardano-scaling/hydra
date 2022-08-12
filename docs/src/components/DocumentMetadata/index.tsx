import React from 'react'
import styles from './styles.module.css'

interface Props {
  lastUpdatedAt: string
  relativeTimeSince: string
  commitHash: string
  lastTranslatedAt?: string
}

export default function DocumentMetadata({
  lastUpdatedAt,
  relativeTimeSince,
  commitHash,
  lastTranslatedAt
}: Props): JSX.Element {

  const link =
    `https://github.com/input-output-hk/hydra-poc/commit/${commitHash}`

  const isPlaceholder = (date: string): boolean =>
    date.startsWith("{{") && date.endsWith("}}")

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

  return (
    <div className={styles.block}>
      {!isPlaceholder(lastUpdatedAt) &&
        renderLastUpdatedAt(
          lastUpdatedAt,
          relativeTimeSince
        )
      }
      {!isPlaceholder(commitHash) &&
        renderCommitHash(commitHash)
      }
      {lastTranslatedAt && !isPlaceholder(lastTranslatedAt) &&
        renderLastTranslatedAt(lastTranslatedAt)
      }
    </div >
  )
}
