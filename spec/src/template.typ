// Document template: page setup, fonts, title page, ToC, theorem environments,
// and Agda code-block rendering. Ported from preamble.tex.
//
// Theorems are non-floating figures with custom `kind`s, which gives per-kind
// numbering and working cross-references (@label) without any external package,
// keeping the Nix build offline-safe.

// === Theorem environments ===

// Shared "theorem" kind: theorem, lemma, corollary, proposition, claim, axiom.
// Separate kinds get independent counters: definition, invariant, postulate,
// example, construction.

#let thmbox(kind, supplement, body, name: none, italic: true) = {
  let head = strong[#supplement #context {
    let c = counter(figure.where(kind: kind))
    c.display()
  }]
  figure(
    kind: kind,
    supplement: supplement,
    numbering: "1",
    placement: none,
    {
      set align(left)
      block(width: 100%, breakable: true, {
        head
        if name != none [ (#name)]
        [. ]
        if italic { emph(body) } else { body }
      })
    },
  )
}

#let theorem(body, name: none) = thmbox("theorem", "Theorem", body, name: name)
#let lemma(body, name: none) = thmbox("theorem", "Lemma", body, name: name)
#let corollary(body, name: none) = thmbox("theorem", "Corollary", body, name: name)
#let proposition(body, name: none) = thmbox("theorem", "Proposition", body, name: name)
#let claim(body, name: none) = thmbox("theorem", "Claim", body, name: name)
#let axiom(body, name: none) = thmbox("theorem", "Axiom", body, name: name)

#let definition(body, name: none) = thmbox("definition", "Definition", body, name: name, italic: false)
#let invariant(body, name: none) = thmbox("invariant", "Invariant", body, name: name, italic: false)
#let postulate(body, name: none) = thmbox("postulate", "Postulate", body, name: name)
#let example(body, name: none) = thmbox("example", "Example", body, name: name, italic: false)
#let construction(body, name: none) = thmbox("construction", "Construction", body, name: name, italic: false)

// Inline editorial note (ported from \todo).
#let todo(body) = box(
  fill: rgb("#fff3b0"),
  inset: (x: 3pt, y: 1pt),
  outset: (y: 1pt),
  radius: 1pt,
  text(size: 8pt, [*TODO:* #body]),
)

#let proof(body) = block(width: 100%, {
  emph[Proof.]
  [ ]
  body
  h(1fr)
  $square$
})

// Pseudocode keyword (for the off-chain protocol listings): bold small-caps.
#let kw(name) = text(weight: "bold", smallcaps(name))

// A titled pseudocode routine block; `body` is the indented routine content.
#let routine(title, body) = block(
  width: 100%,
  stroke: 0.5pt + luma(160),
  inset: 6pt,
  radius: 2pt,
  breakable: false,
  {
    block(below: 4pt, strong(title))
    pad(left: 0.6em, body)
  },
)

// === Agda block relocation (rendered in the appendix, not inline) ===
// The literate `.lagda.typ` sources are typechecked in place by Agda, but to keep the spec body
// readable the rendered code blocks are COLLECTED and shown in an appendix. The body keeps an
// in-place link to the appendix where each block sat. `agda-appendix-mode` flips the show rule from
// "capture + link" (body) to "render framed" (appendix); state is positional, so blocks before the
// flip are captured and those after it (the appendix listing) render normally.
#let agda-appendix-mode = state("agda-appendix-mode", false)

#let render-agda(it) = block(
  fill: luma(245),
  inset: 8pt,
  radius: 3pt,
  width: 100%,
  text(font: "StrippedJuliaMono", size: 8pt, it),
)

// === Document configuration ===

#let hydra-spec(title: none, subtitle: none, authors: (), body) = {
  set page(paper: "a4", margin: (x: 1in, y: 1in), numbering: "1")
  set par(justify: true, leading: 0.65em)
  set text(size: 11pt, font: "New Computer Modern")
  show math.equation: set text(font: "New Computer Modern Math")
  set heading(numbering: "1.1")

  // Coloured, underlined hyperlinks (internal refs, citations and URLs all
  // render as links, so this catches them too).
  show link: it => underline(text(fill: rgb("#1a4fb4"), it))
  // Table-of-contents entries are not `link` elements, so colour them directly.
  show outline.entry: set text(fill: rgb("#1a4fb4"))

  // Theorem figures: render left-aligned (override default centered caption look)
  // and use the shared "theorem" counter for the theorem-family kinds.
  show figure: it => it
  set figure(numbering: "1")
  // Number theorem-family kinds from a single shared counter.
  show figure.where(kind: "theorem"): set figure(supplement: none)

  // Cross-references to theorem figures: "Lemma 3" etc.
  show ref: it => {
    let el = it.element
    if el != none and el.func() == figure and el.kind in (
      "theorem", "definition", "invariant", "postulate", "example", "construction",
    ) {
      let sup = el.supplement
      link(it.target)[#sup #numbering(el.numbering, ..counter(figure.where(kind: el.kind)).at(el.location()))]
    } else {
      it
    }
  }

  // Syntactic highlighting for Agda code via a vendored sublime-syntax grammar
  // (Agda has no Typst backend; this colours keywords/comments/strings/literals).
  set raw(syntaxes: "/agda.sublime-syntax")

  // Agda code blocks: ```agda ... ``` are typechecked in place by Agda. In the BODY they are not
  // shown inline — each is captured (via labelled `metadata`, tagged with its section) and replaced by
  // a link to the appendix; in the APPENDIX listing (`agda-appendix-mode` on) they render framed.
  show raw.where(block: true, lang: "agda"): it => context {
    if agda-appendix-mode.get() {
      render-agda(it)
    } else {
      let hs = query(selector(heading).before(here()))
      let secbody = if hs.len() > 0 { hs.last().body } else { [Front matter] }
      [#metadata((src: it.text, secnum: counter(heading).get(), sec: secbody)) #label("agda-src")]
      block(above: 0.5em, below: 0.8em, align(right, text(
        size: 8pt, fill: rgb("#1a4fb4"),
        link(<agda-appendix>)[_(Agda formalisation in appendix)_],
      )))
    }
  }
  // Bare ``` ... ``` blocks are typechecked by Agda but HIDDEN in the rendered
  // document (module declarations, imports). See Main.lagda.typ.
  show raw.where(block: true, lang: none): none

  set raw(lang: none)

  // Title page
  align(center + horizon, {
    text(size: 20pt, weight: "bold", title)
    if subtitle != none {
      v(2em)
      text(size: 14pt, subtitle)
    }
    v(3em)
    grid(
      columns: (1fr, 1fr),
      gutter: 1.5em,
      ..authors.map(a => align(center, {
        text(weight: "bold", a.name)
        if "email" in a {
          linebreak()
          raw(a.email)
        }
      })),
    )
  })

  pagebreak()

  outline(depth: 2)

  pagebreak()

  body
}
