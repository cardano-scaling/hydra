import { DocSearch } from '@docsearch/react'
import '@docsearch/css'

export default function NavbarSearch({ children, className }) {
  return (
    <DocSearch
      appId="***"
      apiKey="***"
      indexName="hydra"
      transformItems={(items) => {
        if (typeof window === 'undefined') return items

        const isUnstable = window.location.pathname.includes('/head-protocol/unstable/')

        return items.filter((item) => {
          const url = item.url || ''
          const isItemUnstable = url.includes('/head-protocol/unstable/')
          return isUnstable ? isUnstable && isItemUnstable : !isItemUnstable
        })
      }}
    />
  )
}
