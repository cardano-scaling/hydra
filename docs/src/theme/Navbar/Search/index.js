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
          url: removeTrailingSlash(item.url, siteConfig.baseUrl),
        }))
      }}
    />
  )
}

// The crawler indexes the stable site with trailing slashes, but the site is
// built with `trailingSlash: false`, so those URLs 404. Strip the slash from the
// path rather than the whole string: most records are per-heading, and there the
// slash sits before the #fragment.
const removeTrailingSlash = (url, baseUrl) => {
  try {
    const parsed = new URL(url)
    const isBaseRoot = parsed.pathname === baseUrl
    if (!isBaseRoot && parsed.pathname.length > 1 && parsed.pathname.endsWith('/')) {
      parsed.pathname = parsed.pathname.slice(0, -1)
    }
    return parsed.toString()
  } catch {
    return url
  }
}
