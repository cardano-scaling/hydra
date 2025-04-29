import useDocusaurusContext from "@docusaurus/useDocusaurusContext"
import { DocSearch } from '@docsearch/react'
import '@docsearch/css'

export default function NavbarSearch({ children, className }) {
  const { siteConfig } = useDocusaurusContext()

  return (
    <DocSearch
      appId={siteConfig.customFields.docsearchAppId}
      apiKey={siteConfig.customFields.docsearchApiKey}
      indexName="hydra"
      transformItems={(items) => {
        if (typeof window === 'undefined') return items

        const isUnstable = window.location.pathname.includes('/head-protocol/unstable/')

        return items.filter((item) => {
          const url = item.url || ''
          const isItemUnstable = url.includes('/head-protocol/unstable/')
          return isUnstable ? isItemUnstable : !isItemUnstable
        }).map((item) => ({
          ...item,
          url: removeTrailingSlash(item.url),
        }))
      }}
    />
  )
}

const removeTrailingSlash = url => {
  return url.endsWith('/') ? url.slice(0, -1) : url
}
