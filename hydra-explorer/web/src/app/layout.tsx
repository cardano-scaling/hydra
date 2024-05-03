import type { Metadata } from "next"
import "./globals.css"

export const metadata: Metadata = {
  title: "Hydrascan",
  description: "hydra-explorer UI",
}

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode
}>) {
  return (
    <html lang="en">
      <body>
        <link rel="icon" href="/favicon.svg?v=1" type="image/svg+xml" />
        {children}
      </body>
    </html>
  )
}
