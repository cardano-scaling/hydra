"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[5232],{13557:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>o,contentTitle:()=>c,default:()=>h,frontMatter:()=>a,metadata:()=>i,toc:()=>l});var i=n(92835),s=n(74848),r=n(28453);const a={slug:2,title:"2. Reactive Core\n",authors:[],tags:["Accepted"]},c=void 0,o={authorsImageUrls:[]},l=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Decision",id:"decision",level:2},{value:"Consequences",id:"consequences",level:2}];function d(e){const t={a:"a",code:"code",em:"em",h2:"h2",li:"li",ol:"ol",p:"p",...(0,r.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(t.h2,{id:"status",children:"Status"}),"\n",(0,s.jsx)(t.p,{children:"Accepted"}),"\n",(0,s.jsx)(t.h2,{id:"context",children:"Context"}),"\n",(0,s.jsx)(t.p,{children:"We are looking for a way of expressing the Hydra Head protocol logic in a Hydra node."}),"\n",(0,s.jsxs)(t.p,{children:["The Hydra Head protocol is defined as a ",(0,s.jsx)(t.em,{children:"State machine"})," in the paper, whose transitions are inputs that come from different sources which can emit outputs to other instances of the state machine or the mainchain. See the ",(0,s.jsx)(t.a,{href:"https://iohk.io/en/research/library/papers/hydrafast-isomorphic-state-channels/",children:"FC2021"})," paper for details."]}),"\n",(0,s.jsx)(t.p,{children:"It should also be easy to review / give feedback to researchers."}),"\n",(0,s.jsxs)(t.p,{children:["We are familiar with React's ",(0,s.jsx)(t.a,{href:"https://react-redux.js.org/",children:"redux"})," way of structuring applications, which is inspired by ",(0,s.jsx)(t.a,{href:"https://guide.elm-lang.org/architecture/",children:"The Elm Architecture"}),", which itself is a simplification of ",(0,s.jsx)(t.a,{href:"https://en.wikipedia.org/wiki/Functional_reactive_programming",children:"Functional Reactive Programming"})," principles."]}),"\n",(0,s.jsxs)(t.p,{children:["We have experienced benefits with ",(0,s.jsx)(t.em,{children:"Event Sourcing"})," in the domain of persistence in the past."]}),"\n",(0,s.jsx)(t.h2,{id:"decision",children:"Decision"}),"\n",(0,s.jsxs)(t.p,{children:["Implements the Hydra Head core logic as a ",(0,s.jsx)(t.em,{children:"loop"})," that:"]}),"\n",(0,s.jsxs)(t.ol,{children:["\n",(0,s.jsxs)(t.li,{children:["Consumes ",(0,s.jsx)(t.em,{children:"input events"})," from an event ",(0,s.jsx)(t.em,{children:"queue"})]}),"\n",(0,s.jsxs)(t.li,{children:["Applies each ",(0,s.jsx)(t.em,{children:"event"})," to the current ",(0,s.jsx)(t.em,{children:"state"})," yielding potentially an ",(0,s.jsx)(t.em,{children:"updated state"})," and a sequence of ",(0,s.jsx)(t.em,{children:"effects"})]}),"\n",(0,s.jsxs)(t.li,{children:["Execute all ",(0,s.jsx)(t.em,{children:"effects"}),"."]}),"\n"]}),"\n",(0,s.jsx)(t.h2,{id:"consequences",children:"Consequences"}),"\n",(0,s.jsxs)(t.p,{children:["The internal state is only ever changed through ",(0,s.jsx)(t.em,{children:"Events"}),"."]}),"\n",(0,s.jsxs)(t.p,{children:["The core state machine ",(0,s.jsx)(t.em,{children:"transition"})," function ",(0,s.jsx)(t.em,{children:"is pure"})," and reviewing it requires minimal Haskell knowledge."]}),"\n",(0,s.jsxs)(t.p,{children:["Side effects are all handled at the ",(0,s.jsx)(t.code,{children:"node"})," level."]})]})}function h(e={}){const{wrapper:t}={...(0,r.R)(),...e.components};return t?(0,s.jsx)(t,{...e,children:(0,s.jsx)(d,{...e})}):d(e)}},28453:(e,t,n)=>{n.d(t,{R:()=>a,x:()=>c});var i=n(96540);const s={},r=i.createContext(s);function a(e){const t=i.useContext(r);return i.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function c(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:a(e.components),i.createElement(r.Provider,{value:t},e.children)}},92835:e=>{e.exports=JSON.parse('{"permalink":"/head-protocol/adr/2","source":"@site/adr/2021-06-06_002-reactive-core.md","title":"2. Reactive Core\\n","description":"Status","date":"2021-06-06T00:00:00.000Z","tags":[{"inline":true,"label":"Accepted","permalink":"/head-protocol/adr/tags/accepted"}],"readingTime":0.975,"hasTruncateMarker":false,"authors":[],"frontMatter":{"slug":"2","title":"2. Reactive Core\\n","authors":[],"tags":["Accepted"]},"unlisted":false,"prevItem":{"title":"1. Record Architecture Decisions\\n","permalink":"/head-protocol/adr/1"},"nextItem":{"title":"3. Asynchronous Duplex Client API","permalink":"/head-protocol/adr/3"}}')}}]);