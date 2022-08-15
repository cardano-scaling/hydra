import React, { useEffect, useState } from 'react'
import styles from './styles.module.css'

interface Props {
  path?: string
}

interface Metadata {
  lastUpdatedAt: string
  relativeTimeSince: string
  commitHash: string
  lastTranslatedAt?: string
}

export default function DocumentMetadata({ path }: Props): JSX.Element {

  const defaultMetadata: Metadata = {
    lastUpdatedAt: "{{last-updated-at}}",
    relativeTimeSince: "{{relative-time-since}}",
    commitHash: "{{commit-hash}}",
    lastTranslatedAt: "{{last-translated-at}}"
  }

  const baseDocumentApiURL =
    'https://api.github.com/repos/input-output-hk/hydra-poc/commits/master?path='
  const baseI18NApiURL =
    'https://api.github.com/repos/input-output-hk/hydra-poc/commits/master?path=i18n'

  const [error, setError] = useState(null)
  const [jsonDocumentResult, setJsonDocumentResult] = useState(defaultMetadata)
  const [jsonI18NResult, setI18NJsonResult] = useState(defaultMetadata)

  const buildEffectApiCallGH = (url: string, callback: (result: Metadata) => void) => {
    useEffect(() => {
      fetch(url)
        .then(res => res.json())
        .then(
          (result) => {
            const metadata: Metadata = {
              lastUpdatedAt: result["commit"]["committer"]["date"]
              , relativeTimeSince: "todo"
              , commitHash: result["commit"]["tree"]["sha"]
              , lastTranslatedAt: result["commit"]["committer"]["date"]
            }
            callback(metadata)
          },
          (error) => {
            setError(error)
            console.log(error)
          }
        )
    }, [])
  }

  buildEffectApiCallGH(baseDocumentApiURL + path, setJsonDocumentResult)
  buildEffectApiCallGH(baseI18NApiURL, setI18NJsonResult)

  const link =
    `https://github.com/input-output-hk/hydra-poc/commit/${jsonDocumentResult.commitHash}`

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


  const { lastUpdatedAt, relativeTimeSince, commitHash } = jsonDocumentResult
  const { lastTranslatedAt } = jsonI18NResult
  const isPathDefined = !(path === undefined) && (error === null)

  return (
    <div className={styles.block}>
      {!isPlaceholder(lastUpdatedAt) && isPathDefined &&
        renderLastUpdatedAt(
          lastUpdatedAt,
          relativeTimeSince
        )
      }
      {!isPlaceholder(commitHash) && isPathDefined &&
        renderCommitHash(commitHash)
      }
      {lastTranslatedAt && !isPlaceholder(lastTranslatedAt) && isPathDefined &&
        renderLastTranslatedAt(lastTranslatedAt)
      }
    </div >
  )
}
