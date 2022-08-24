import React from "react"
import docsMetadataJson from "@site/static/docs-metadata.json"
import moment from "moment"
import useIsBrowser from '@docusaurus/useIsBrowser'
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';

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
  let link = `https://github.com/input-output-hk/hydra-poc/commit/${commitHash}`
  return (
    <p>
      Last updated:&nbsp;
      <a href={link}>
        {moment(lastUpdatedAt).fromNow()}
      </a>
    </p>
  )
}

const renderTranslatedMetadata = ({
  sourceUpdatedAt,
  translationUpdatedAt,
  commitHash,
}: TranslatedMetadata) => {
  let link = `https://github.com/input-output-hk/hydra-poc/commit/${commitHash}`
  const diffMs = moment(translationUpdatedAt).diff(sourceUpdatedAt)
  const outdated = diffMs < 0
  const maybeRenderWarning = outdated &&
    (<b>(⚠️ Warning:&nbsp; {moment.duration(diffMs).humanize()} behind default language)</b>)
  return (
    <p>
      Translation updated:&nbsp;
      <a href={link}>
        {moment(translationUpdatedAt).fromNow()}
        {maybeRenderWarning}
      </a>
    </p>
  )
}

export default function DocumentMetadata({ }: Props): JSX.Element {
  const context = useDocusaurusContext();

  const isBrowser = useIsBrowser()

  if (!isBrowser) {
    return <></>
  }

  const baseUrl = context.siteConfig.baseUrl
  const documentPath = new URL(window.location.href).pathname.replace(baseUrl, "")

  const [_, ...restPath] = documentPath.split("/")

  const defaultLocale = context.i18n.defaultLocale
  const currentLocale = context.i18n.currentLocale
  const isTranslatedLanguage = defaultLocale !== currentLocale
  const translatedDocumentPath = restPath.join("/")
  const path = isTranslatedLanguage ? translatedDocumentPath : documentPath

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

  const metadata: Metadata = isTranslatedLanguage
    ? documentMetadata[currentLocale]
    : sourceMetadata

  // do not display if metadata for translated language is not found in docs-metadata.json
  if (metadata === undefined) {
    return <></>
  }

  if (isTranslatedLanguage) {
    const translatedMetadata = {
      sourceUpdatedAt: sourceMetadata.lastUpdatedAt,
      translationUpdatedAt: documentMetadata[currentLocale].lastUpdatedAt,
      commitHash: documentMetadata[currentLocale].commitHash,
    }
    return renderTranslatedMetadata(translatedMetadata)
  } else {
    return renderMetadata(metadata)
  }
}
