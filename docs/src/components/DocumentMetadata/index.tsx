import React, { useEffect, useState } from 'react'
import styles from './styles.module.css'
import metadatas from '@site/static/metadatas.json'

interface Props { }

interface Metadata {
  lastUpdatedAt: string
  relativeTimeSince: string
  commitHash: string
}

const Utils = {
  getRelativeMillisTimeSince: (lastUpdatedAt: string, lastTranslatedAt: string) => {
    const docLastUpdatedAt = new Date(lastUpdatedAt).getTime()
    const docLastTranslatedAt = new Date(lastTranslatedAt).getTime()
    const relativeTimeSince = docLastTranslatedAt - docLastUpdatedAt
    return relativeTimeSince
  }
  , getTimeObject: (relativeTime: number) => {
    const relativeUnits = [
      { unit: 'seconds', divisor: 1000 },
      { unit: 'minutes', divisor: 1000 * 60 },
      { unit: 'hours', divisor: 1000 * 60 * 60 },
      { unit: 'days', divisor: 1000 * 60 * 60 * 24 }
    ]

    const relativeTimes = relativeUnits
      .map(({ unit, divisor }) => {
        const value = Math.floor(relativeTime / divisor)
        return { value, unit }
      })

    return relativeTimes.reduce((acc, obj) => {
      return (
        // discard when relative time of translated >= default language
        obj.value <= 0 &&
        // we are interested to find the maximum negative relative time in the list
        obj.value >= acc.value
      ) ? obj : acc
    })
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

    // do not display `last translated at` on default language
    if (!languages.includes(language)) {
      return <></>
    }

    const defaultMetadata: Metadata = metadatas[englishPath.join("/")]

    // do not display `last translated at` for those who have not a default language reference
    if (defaultMetadata === undefined) {
      return <></>
    }

    const relativeMillisTimeSince =
      Utils.getRelativeMillisTimeSince(defaultMetadata.lastUpdatedAt, lastTranslatedAt)

    const timeObject =
      Utils.getTimeObject(relativeMillisTimeSince)

    // dont display warning on translated pages when up to date or above
    if (timeObject.value >= 0) {
      return <></>
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
