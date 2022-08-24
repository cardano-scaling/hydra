import React, { useEffect, useState } from 'react'
import styles from './styles.module.css'
import docsMetadataJson from '@site/static/docs-metadata.json'
import moment from 'moment'

interface Props { }

interface Metadata {
  lastUpdatedAt: string
  commitHash: string
}

interface TranslatedMetadata {
  sourceUpdatedAt: string
  translationUpdatedAt: string
  commitHash: string
}

const renderMetadata = ({ lastUpdatedAt, commitHash }: Metadata) => {
  let link = `https://github.com/input-output-hk/hydra-poc/commit/${commitHash}`;
  return <div>
    Last updated: <a href={link}><b>{moment(lastUpdatedAt).fromNow()}</b></a>
  </div>
}

const renderTranslatedMetadata = ({ sourceUpdatedAt, translationUpdatedAt, commitHash }: TranslatedMetadata) => {
  let link = `https://github.com/input-output-hk/hydra-poc/commit/${commitHash}`;
  const diff = moment(translationUpdatedAt).diff(sourceUpdatedAt)

  // dont display warning on translated pages when up to date or above
  if (diff >= 0) {
    return <div>
      Translation updated at: <a href={link}><b>{moment(translationUpdatedAt).fromNow()}</b></a><br />
      Translation ahead source: <b>{moment(sourceUpdatedAt).from(translationUpdatedAt)}</b>
      (⚠️ Warning: <b>{diff}</b> ahead default language)
    </div>
  }
  else {
    return <div>
      Translation updated at: <a href={link}><b>{moment(translationUpdatedAt).fromNow()}</b></a><br />
      Translation behind source: <b>{moment(sourceUpdatedAt).from(translationUpdatedAt)}</b>
      (⚠️ Warning: <b>{diff}</b> behind default language)
    </div>
  }
}

export default function DocumentMetadata({ }: Props): JSX.Element {
  const [documentPath, setDocumentPath] = useState('placeholder')

  useEffect(() => {
    if (window) {
      const path = new URL(window.location.href).pathname.replace('/head-protocol/', '')
      setDocumentPath('fr/' + path)
    }
  }, [])

  // do not display metadata if page cannot access its location from the window.location.href
  // this is the case when the page is not rendered in the browser, like during `yarn build`
  if (documentPath == 'placeholder') {
    return <></>
  }

  const [maybeLanguage, ...restPath] = documentPath.split("/")
  const supportedLanguages = ['fr', 'ja'] //@TODO move to config
  const isTranslatedLanguage = supportedLanguages.includes(maybeLanguage)

  const path = isTranslatedLanguage ? restPath.join("/") : documentPath

  // do not display if document path is not found in docs-metadata.json
  if (docsMetadataJson[path] === undefined) {
    return <></>
  }

  const documentMetadata = docsMetadataJson[path]
  const sourceMetadata = documentMetadata["source"]

  // do not display if metadata for source language is not found in docs-metadata.json
  if (sourceMetadata === undefined) {
    return <></>
  }

  const metadata: Metadata = isTranslatedLanguage ? documentMetadata[maybeLanguage] : sourceMetadata

  // do not display if metadata for translated language is not found in docs-metadata.json
  if (metadata === undefined) {
    return <></>
  }

  if (isTranslatedLanguage) {
    const translatedMetadata = {
      sourceUpdatedAt: sourceMetadata.lastUpdatedAt,
      translationUpdatedAt: documentMetadata[maybeLanguage].lastUpdatedAt,
      commitHash: documentMetadata[maybeLanguage].commitHash
    }
    return renderTranslatedMetadata(translatedMetadata)
  } else {
    return renderMetadata(metadata)
  }
}
