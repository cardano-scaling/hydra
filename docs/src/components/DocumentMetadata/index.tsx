import React, { type ReactNode } from 'react'
import styles from './styles.module.css'

interface Props {
  lastUpdatedAt: string
  commitHash: string
  lastTranslatedAt?: string
}

const isNumber = (str: string): boolean =>
  !isNaN(parseInt(str))

const isValidDate = (date: string): boolean =>
  isNumber(date) && isNaN(Date.parse(date))

const isPlaceholder = (date: string): boolean =>
  date.startsWith("{{") && date.endsWith("}}")

const dateToFormatString = (date: string): string =>
  new Date(parseInt(date)).toUTCString().replace("GMT", "")

const daysSince = (dateSeconds: number): number =>
  Math.floor((new Date().getTime() - dateSeconds * 1000) / 8.64e7)

export default function DocumentMetadata({
  lastUpdatedAt,
  commitHash,
  lastTranslatedAt
}: Props): JSX.Element {

  const link =
    `https://github.com/input-output-hk/hydra-poc/commit/${commitHash}`

  const renderLastUpdatedAt = (lastUpdatedAtDate: string, daysSinceLastUpdatedAt: number) =>
    <i className={styles.info}>
      Last updated at: <b>{lastUpdatedAtDate}</b> (
      <b>{daysSinceLastUpdatedAt}</b> days since last change
      )
    </i>

  const renderCommitHash = (commitHash: string) =>
    <i className={styles.info}>Last commit: <a href={link}><b>{commitHash}</b></a></i>

  const renderLastTranslatedAt = (lastTranslatedAt: string) =>
    <i className={styles.info}>Last translated at: <b>{lastTranslatedAt}</b></i>

  return (
    <div className={styles.block}>
      {isValidDate(lastUpdatedAt) &&
        renderLastUpdatedAt(
          dateToFormatString(lastUpdatedAt),
          daysSince(parseInt(lastUpdatedAt))
        )
      }
      {!isPlaceholder(commitHash) &&
        renderCommitHash(commitHash)
      }
      {lastTranslatedAt && isValidDate(lastTranslatedAt) &&
        renderLastTranslatedAt(dateToFormatString(lastTranslatedAt))
      }
    </div >
  )
}