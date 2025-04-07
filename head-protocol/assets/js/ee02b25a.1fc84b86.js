"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[4289],{81611:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>c,contentTitle:()=>i,default:()=>h,frontMatter:()=>o,metadata:()=>s,toc:()=>l});var s=n(26342),a=n(74848),r=n(28453);const o={slug:13,title:"13. Plutus Contracts Testing Strategy\n",authors:[],tags:["Accepted"]},i=void 0,c={authorsImageUrls:[]},l=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Decision",id:"decision",level:2},{value:"Consequences",id:"consequences",level:2}];function d(e){const t={a:"a",code:"code",em:"em",h2:"h2",li:"li",p:"p",ul:"ul",...(0,r.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(t.h2,{id:"status",children:"Status"}),"\n",(0,a.jsx)(t.p,{children:"Accepted"}),"\n",(0,a.jsx)(t.h2,{id:"context",children:"Context"}),"\n",(0,a.jsxs)(t.ul,{children:["\n",(0,a.jsxs)(t.li,{children:["We are implementing our custom (",(0,a.jsx)(t.a,{href:"/adr/10",children:"Direct"}),") interaction w/ Cardano blockchain and not using the PAB nor the ",(0,a.jsx)(t.code,{children:"Contract"})," monad to define off-chain contract code"]}),"\n",(0,a.jsxs)(t.li,{children:["This implies we cannot use the ",(0,a.jsx)(t.a,{href:"https://github.com/input-output-hk/plutus-apps/blob/main/plutus-contract/src/Plutus/Contract/Test.hs",children:"official"})," testing framework for Contracts which relies on ",(0,a.jsx)(t.code,{children:"Contract"})," monad and emulator traces nor the ",(0,a.jsx)(t.a,{href:"https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-testing.html",children:"QuickCheck based framework"})]}),"\n",(0,a.jsxs)(t.li,{children:["We want to follow our ",(0,a.jsx)(t.a,{href:"/adr/12",children:"Test-Driven Development"})," approach for contracts as this is a critical part of Hydra"]}),"\n",(0,a.jsx)(t.li,{children:"On-Chain Validators need not only to be correct and functional, but also secure and hardened against malicious parties"}),"\n"]}),"\n",(0,a.jsx)(t.h2,{id:"decision",children:"Decision"}),"\n",(0,a.jsx)(t.p,{children:(0,a.jsx)(t.em,{children:"Therefore"})}),"\n",(0,a.jsxs)(t.ul,{children:["\n",(0,a.jsxs)(t.li,{children:["We test-drive single contracts code using ",(0,a.jsx)(t.em,{children:"Mutation-Based Property Testing"})]}),"\n",(0,a.jsxs)(t.li,{children:["Contracts are tested through the construction of actual ",(0,a.jsx)(t.em,{children:"transactions"})," and running phase-2 ledger validation process"]}),"\n",(0,a.jsx)(t.li,{children:'We start from a "healthy" transaction, that\'s expected to be correct and stay so'}),"\n",(0,a.jsxs)(t.li,{children:["Contract code is initially ",(0,a.jsx)(t.code,{children:"const True"})," function that validates any transaction"]}),"\n",(0,a.jsxs)(t.li,{children:["We flesh the contract's code piecemeal through the introduction of ",(0,a.jsx)(t.em,{children:"Mutations"})," that turn a healthy transaction into an expectedly invalid one"]}),"\n",(0,a.jsx)(t.li,{children:"We gradually build a set of combinators and generators that make it easier to mutate arbitrarily transactions, and combine those mutations"}),"\n"]}),"\n",(0,a.jsx)(t.h2,{id:"consequences",children:"Consequences"}),"\n",(0,a.jsxs)(t.ul,{children:["\n",(0,a.jsxs)(t.li,{children:["We make the contracts' ",(0,a.jsx)(t.em,{children:"Threat model"}),"  explicit through the tests we write, which should help future auditors' work"]}),"\n",(0,a.jsxs)(t.li,{children:["We'll need an additional layer of tests to exercise the Hydra OCV State Machine through ",(0,a.jsx)(t.em,{children:"sequence of transactions"}),". This could be implemented using ",(0,a.jsx)(t.a,{href:"https://github.com/input-output-hk/plutus-apps/tree/main/quickcheck-dynamic",children:"quickcheck-dynamic"})," library, or other tools that are currently being developed by the Cardano community"]}),"\n"]})]})}function h(e={}){const{wrapper:t}={...(0,r.R)(),...e.components};return t?(0,a.jsx)(t,{...e,children:(0,a.jsx)(d,{...e})}):d(e)}},28453:(e,t,n)=>{n.d(t,{R:()=>o,x:()=>i});var s=n(96540);const a={},r=s.createContext(a);function o(e){const t=s.useContext(r);return s.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function i(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:o(e.components),s.createElement(r.Provider,{value:t},e.children)}},26342:e=>{e.exports=JSON.parse('{"permalink":"/head-protocol/adr/13","source":"@site/adr/2022-01-19_013-contract-testing-strategy.md","title":"13. Plutus Contracts Testing Strategy\\n","description":"Status","date":"2022-01-19T00:00:00.000Z","tags":[{"inline":true,"label":"Accepted","permalink":"/head-protocol/adr/tags/accepted"}],"readingTime":1.26,"hasTruncateMarker":false,"authors":[],"frontMatter":{"slug":"13","title":"13. Plutus Contracts Testing Strategy\\n","authors":[],"tags":["Accepted"]},"unlisted":false,"prevItem":{"title":"12. Top-down Test-driven Design\\n","permalink":"/head-protocol/adr/12"},"nextItem":{"title":"14. Token usage in Hydra Scripts\\n","permalink":"/head-protocol/adr/14"}}')}}]);