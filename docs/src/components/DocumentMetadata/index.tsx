import React, { useEffect, useState } from 'react'
import styles from './styles.module.css'
import metadatas from '@site/static/docs-metadata.json'
import moment from 'moment'

interface Props { }

interface Metadata {
  lastUpdatedAt: string
  relativeTimeSince: string
  commitHash: string
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

    // do not display `last translated at` on default language
    if (!languages.includes(language)) {
      return <></>
    }

    const defaultMetadata: Metadata = metadatas[englishPath.join("/")]

    // do not display `last translated at` for those who have not a default language reference
    if (defaultMetadata === undefined) {
      return <></>
    }


    const translationChangedAt = moment(lastTranslatedAt)
    const sourceChangedAt = moment(defaultMetadata.lastUpdatedAt)
    const relativeTimeSince = translationChangedAt.from(sourceChangedAt, true)
    const diff = translationChangedAt.diff(sourceChangedAt)

    // dont display warning on translated pages when up to date or above
    if (diff >= 0) {
      return <></>
    }

    return <i className={styles.info}>
      Last translated at: <b>{lastTranslatedAt}</b>
      (⚠️ Warning: <b>{relativeTimeSince}</b> behind default language)
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

  // do not display metadata if page cannot access its location from the window.location.href
  // this is the case when the page is not rendered in the browser, like during `yarn build`
  if (documentPath == 'placeholder') {
    return <></>
  }

  const metadata: Metadata = metadatas[documentPath]

  // do not display metadata if not found in metadatas.json
  if (metadata === undefined) {
    return <></>
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
