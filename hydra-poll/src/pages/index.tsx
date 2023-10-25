import Main from "./poll/main"

export default function Home() {
  return (
    <div className="container">
      <header className="title">
        <title>Hydra Poll</title>
        <meta name="description" content="Poll running on Hydra Head protocol" />
      </header>

      <main className="main">
        <Main />
      </main>

      <footer className="footer">
      </footer>
    </div>
  )
}
