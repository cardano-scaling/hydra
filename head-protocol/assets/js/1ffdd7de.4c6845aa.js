"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[7782],{95190:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>d,contentTitle:()=>o,default:()=>h,frontMatter:()=>i,metadata:()=>s,toc:()=>l});const s=JSON.parse('{"id":"faqs","title":"FAQs","description":"What is Hydra?","source":"@site/docs/faqs.md","sourceDirName":".","slug":"/faqs","permalink":"/head-protocol/docs/faqs","draft":false,"unlisted":false,"editUrl":"https://github.com/cardano-scaling/hydra/tree/master/docs/docs/faqs.md","tags":[],"version":"current","frontMatter":{"title":"FAQs"},"sidebar":"userDocumentation","previous":{"title":"Commit funds to an open Head","permalink":"/head-protocol/docs/how-to/incremental-commit"},"next":{"title":"Get involved","permalink":"/head-protocol/docs/get-involved"}}');var a=t(74848),r=t(28453);const i={title:"FAQs"},o="Frequently asked questions",d={},l=[{value:"Examples",id:"examples",level:3}];function c(e){const n={a:"a",code:"code",em:"em",h1:"h1",h3:"h3",header:"header",li:"li",ol:"ol",p:"p",strong:"strong",ul:"ul",...(0,r.R)(),...e.components},{Details:t}=n;return t||function(e,n){throw new Error("Expected "+(n?"component":"object")+" `"+e+"` to be defined: you likely forgot to import, pass, or provide it.")}("Details",!0),(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(n.header,{children:(0,a.jsx)(n.h1,{id:"frequently-asked-questions",children:"Frequently asked questions"})}),"\n",(0,a.jsxs)(t,{children:[(0,a.jsx)("summary",{children:"What is Hydra?"}),(0,a.jsx)(n.p,{children:"Hydra is a family of layer 2 protocols designed to address network scalability\ncapabilities. Hydra Head is the first in this protocol suite, providing the\nfoundation on which to build out further scalability."})]}),"\n",(0,a.jsxs)(t,{children:[(0,a.jsx)("summary",{children:"When Hydra?"}),(0,a.jsxs)(n.p,{children:["The project is available on all Cardano networks (including mainnet), and\nreleases with new features become available every four to six weeks. The\nroadmap is publicly available on ",(0,a.jsx)("a",{href:"https://github.com/orgs/input-output-hk/projects/21/views/25",children:"GitHub."})]})]}),"\n",(0,a.jsxs)(t,{children:[(0,a.jsx)("summary",{children:"What is the difference between layer 1 and layer 2?"}),(0,a.jsxs)(n.p,{children:["Layer 1 solutions provide the foundational infrastructure of a blockchain\nnetwork, while layer 2 solutions introduce supplementary protocols or\nmechanisms to improve scalability and functionality ",(0,a.jsx)("a",{href:"https://www.essentialcardano.io/article/layer-1-and-layer-2-all-you-need-to-know",children:"Read\nmore in this blog post."})]})]}),"\n",(0,a.jsxs)(t,{children:[(0,a.jsx)("summary",{children:"Is the Hydra Head protocol secure?"}),(0,a.jsx)(n.p,{children:"Absolutely. Hydra protocols were born out of IOG research, got peer-reviewed,\nand are implemented using test-driven development. The Hydra Head protocol\nis a true layer 2 and can fall back directly onto the Cardano layer 1 \u2013\nhence inheriting the security model of the Cardano blockchain."})]}),"\n",(0,a.jsxs)(t,{children:[(0,a.jsx)("summary",{children:"So what\u2019s this I have heard about \u20181M TPS\u2019?"}),(0,a.jsxs)(n.p,{children:["This has been previously referenced as a theoretical maximum, but the reality\nis more nuanced. For a start, with Cardano\u2019s \u2018transactions within\ntransactions\u2019 EUTXO model, TPS itself isn\u2019t a useful metric. A Hydra Head is\nlike a small community within a larger group. Initially, these communities\noperate independently. So, adding up their metrics to get a total picture\nisn't accurate. Since Hydra heads use the EUTXO model, they can process\ntransactions simultaneously without conflicts, especially with good\nnetworking, which optimizes resource usage. As the project progresses, we're\nconstantly evaluating its real-world performance in terms of throughput and\nfinality. For more details, read ",(0,a.jsx)("a",{href:"https://example.com/more-info",children:"this"})," blog post and see the latest\nbenchmarking data ",(0,a.jsx)("a",{href:"https://example.com/latest-data",children:"here"}),"."]})]}),"\n",(0,a.jsxs)(t,{children:[(0,a.jsx)("summary",{children:"Can anyone use the Hydra Head protocol?"}),(0,a.jsx)(n.p,{children:"Yes, it's designed to be accessible for developers and end users alike,\nrequiring minimal changes to existing applications to integrate with the\nprotocol. However, it is important to note that Hydra is not a network\nupgrade, and it's not like flipping a switch on Cardano to make it fast -\ninstead, applications need to adopt and build on Hydra heads to benefit from\nit."})]}),"\n",(0,a.jsxs)(t,{children:[(0,a.jsx)("summary",{children:"When is the Hydra Head protocol a good fit?"}),(0,a.jsx)(n.p,{children:"The Hydra Head protocol is well-suited for situations where a known set of\nparticipants know each other well enough to agree on building a network but\ndon\u2019t trust each other enough to manage funds without securing their assets.\nThis security is backed by the possibility of settling disputes on layer 1."})]}),"\n",(0,a.jsxs)(t,{children:[(0,a.jsx)("summary",{children:"Can I run Plutus scripts inside a head?"}),(0,a.jsx)(n.p,{children:"Yes! Transactions running between head participants are fully developed Alonzo\ntransactions. They carry scripts and spend UTXOs in the same manner as layer 1\ntransactions. Each Hydra node runs a Cardano ledger and maintains a ledger\nstate."})]}),"\n",(0,a.jsxs)(t,{children:[(0,a.jsx)("summary",{children:"Can a third party run a Hydra node on behalf of wallet owners (eg, running managed Hydra heads)?"}),(0,a.jsxs)(n.p,{children:["Totally! This is similar to ",(0,a.jsx)(n.a,{href:"https://phoenix.acinq.co/",children:"Phoenix"})," in Bitcoin\nLightning: a non-custodial managed lightning node. As an end-user, you retain\nfull control over your keys and funds, but the underlying infrastructure is\nmanaged on your behalf (with associated fees). However, this setup requires\nsome level of trust in the service provider to handle contestations and head\nclosures properly."]})]}),"\n",(0,a.jsxs)(t,{children:[(0,a.jsx)("summary",{children:"What is the relationship between Hydra heads and Hydra nodes?"}),(0,a.jsxs)(n.p,{children:["It is (at least*) a ",(0,a.jsx)(n.strong,{children:"one-to-many"})," relationship. Each Hydra head consists of\nseveral Hydra nodes. We currently aim for up to 100 nodes per head as a\nstretch goal. Heads are independent and form isolated networks, allowing for\ninfinitely many heads to run in parallel."]}),(0,a.jsx)(n.p,{children:(0,a.jsx)(n.em,{children:"(*) It is possible for Hydra nodes to support multiple heads, creating a\nmany-to-many relationship."})})]}),"\n",(0,a.jsxs)(t,{children:[(0,a.jsx)("summary",{children:"Is the Hydra Head protocol a sidechain?"}),(0,a.jsx)(n.p,{children:"No, it isn't. There are two crucial reasons why Hydra heads are not considered\nsidechains:"}),(0,a.jsxs)(n.ol,{children:["\n",(0,a.jsxs)(n.li,{children:["\n",(0,a.jsx)(n.p,{children:"There's no guaranteed data availability in Hydra. Transactions are (a) only\nknown to head participants and (b) typically forgotten once processed.\nThere are no blocks in a Hydra head, and participants have no incentive to\nkeep the history or make it available to users outside the head."}),"\n"]}),"\n",(0,a.jsxs)(n.li,{children:["\n",(0,a.jsx)(n.p,{children:"A head network is static; new participants cannot join and must be decided\nupfront. The network is isolated and private, accessible only to a set of\nwell-known participants."}),"\n"]}),"\n"]})]}),"\n",(0,a.jsxs)(t,{children:[(0,a.jsx)("summary",{children:"If the Hydra Head ledger is configured with a non-zero transaction fee, where do the paid fees go?"}),(0,a.jsxs)(n.p,{children:["Setting protocol parameters with ",(0,a.jsx)(n.code,{children:"fee > 0"})," ensures that Hydra Head (layer 2)\ntransactions consume more than they produce. On layer 1, the UTXO remains\nunchanged, and the difference accrues. Currently, when settling an agreed\nstate from layer 2 on layer 1 during fanout, this difference becomes\nspendable by the head participant who posts the ",(0,a.jsx)(n.code,{children:"fanoutTx"}),"."]})]}),"\n",(0,a.jsxs)(t,{children:[(0,a.jsx)("summary",{children:" Is it possible to use different protocol parameters in the Hydra head?"}),(0,a.jsxs)(n.p,{children:["Yes, the ledger used for layer 2 transactions in a Hydra head is configurable,\nallowing for the same or different protocol parameters as those used in\nlayer 1. ",(0,a.jsx)(n.strong,{children:"However, there is an important caveat to consider"}),":"]}),(0,a.jsxs)(n.p,{children:["If UTXOs are snapshotted on layer 2, they must be fanned out on layer 1\n",(0,a.jsx)(n.strong,{children:"exactly"})," as they were recorded in the snapshot."]}),(0,a.jsx)(n.h3,{id:"examples",children:"Examples"}),(0,a.jsxs)(n.ol,{children:["\n",(0,a.jsxs)(n.li,{children:["\n",(0,a.jsxs)(n.p,{children:[(0,a.jsxs)(n.strong,{children:["Minimum UTXO value (",(0,a.jsx)(n.code,{children:"minUTxOValue = 0"}),")"]}),":"]}),"\n",(0,a.jsxs)(n.ul,{children:["\n",(0,a.jsxs)(n.li,{children:["Outputs with no 'ada' on layer 2 would be disallowed on layer 1,\npreventing their fanout. This restriction makes direct fanout impossible\nfor such outputs. Even using partial fanout, as considered in ",(0,a.jsx)(n.a,{href:"https://github.com/cardano-scaling/hydra/issues/190",children:"this\nfeature"}),", would not\npermit the fanout of affected UTXOs."]}),"\n"]}),"\n"]}),"\n",(0,a.jsxs)(n.li,{children:["\n",(0,a.jsxs)(n.p,{children:[(0,a.jsxs)(n.strong,{children:["Maximum transaction execution units (",(0,a.jsx)(n.code,{children:"maxTxExecutionUnits(L2) > maxTxExecutionUnits(L1)"}),")"]}),":"]}),"\n",(0,a.jsxs)(n.ul,{children:["\n",(0,a.jsx)(n.li,{children:"Outputs directed to scripts, which are too costly to validate on layer 1,\ncan still be fanned out but will become unspendable due to exceeding the\nallowable execution limits on layer 1."}),"\n"]}),"\n"]}),"\n"]}),(0,a.jsxs)(n.p,{children:[(0,a.jsx)(n.strong,{children:"Remember"}),", with great power comes great responsibility. It is crucial to\ncarefully manage and align the layer 1 and layer 2 settings to ensure seamless\noperability and avoid unintended consequences in transaction processing."]})]}),"\n",(0,a.jsxs)(t,{children:[(0,a.jsx)("summary",{children:"How do I get involved?"}),(0,a.jsx)(n.p,{children:"Join public monthly meetings to engage with the Hydra team and contribute to\nits open governance. These meetings provide a platform for community\ndevelopers to stay updated on the latest developments, ask questions directly\nto the team, and share their ideas. Start building on Hydra like SundaeLabs,\nModulo-P, Obsidian Systems, MLabs, and others!"})]})]})}function h(e={}){const{wrapper:n}={...(0,r.R)(),...e.components};return n?(0,a.jsx)(n,{...e,children:(0,a.jsx)(c,{...e})}):c(e)}},28453:(e,n,t)=>{t.d(n,{R:()=>i,x:()=>o});var s=t(96540);const a={},r=s.createContext(a);function i(e){const n=s.useContext(r);return s.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function o(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:i(e.components),s.createElement(r.Provider,{value:n},e.children)}}}]);