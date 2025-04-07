"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[6856],{82439:(e,s,n)=>{n.r(s),n.d(s,{assets:()=>i,contentTitle:()=>c,default:()=>h,frontMatter:()=>a,metadata:()=>t,toc:()=>d});var t=n(24489),o=n(74848),r=n(28453);const a={slug:5,title:"5. Use io-classes\n",authors:[],tags:["Accepted"]},c=void 0,i={authorsImageUrls:[]},d=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Decision",id:"decision",level:2},{value:"Consequences",id:"consequences",level:2}];function l(e){const s={a:"a",code:"code",h2:"h2",li:"li",p:"p",ul:"ul",...(0,r.R)(),...e.components};return(0,o.jsxs)(o.Fragment,{children:[(0,o.jsx)(s.h2,{id:"status",children:"Status"}),"\n",(0,o.jsx)(s.p,{children:"Accepted"}),"\n",(0,o.jsx)(s.h2,{id:"context",children:"Context"}),"\n",(0,o.jsxs)(s.p,{children:["Although we try to contain the use of IO at the outskirt of the Hydra node using ",(0,o.jsx)(s.a,{href:"/adr/4",children:"Handle pattern"})," and ",(0,o.jsx)(s.a,{href:"/adr/2",children:"Reactive core"}),", low-level effects are still needed in various places, notably to define concurrently executing actions, and thus need to be tested"]}),"\n",(0,o.jsx)(s.p,{children:"Testing asynchronous and concurrent code is notoriously painful"}),"\n",(0,o.jsxs)(s.p,{children:["The ouroboros consensus test suite and ",(0,o.jsx)(s.a,{href:"https://github.com/cardano-scaling/hydra-sim",children:"hydra-sim"})," simulation have demonstrated the effectiveness of abstracting concurrent primitives through the use of typeclasses (MTL-style pattern) and being able to run these as pure code, harvesting and analysing produced execution traces."]}),"\n",(0,o.jsxs)(s.p,{children:["There are other such libraries, e.g. ",(0,o.jsx)(s.a,{href:"https://hackage.haskell.org/package/concurrency",children:"concurrency"})," and ",(0,o.jsx)(s.a,{href:"https://hackage.haskell.org/package/dejafu",children:"dejafu"}),", as well as the venerable ",(0,o.jsx)(s.a,{href:"https://hackage.haskell.org/package/exceptions",children:"exceptions"})," (for abstracting exception throwing)."]}),"\n",(0,o.jsx)(s.h2,{id:"decision",children:"Decision"}),"\n",(0,o.jsxs)(s.p,{children:["For all IO effects covered by the library, use functions from typeclasses exposed by ",(0,o.jsx)(s.a,{href:"https://github.com/input-output-hk/ouroboros-network/tree/e338f2cf8e1078fbda9555dd2b169c6737ef6774/io-classes",children:"io-classes"}),". As of this writing, this covers:"]}),"\n",(0,o.jsxs)(s.ul,{children:["\n",(0,o.jsxs)(s.li,{children:["All STM operations through ",(0,o.jsx)(s.code,{children:"MonadSTM"})]}),"\n",(0,o.jsxs)(s.li,{children:["Time and timers through ",(0,o.jsx)(s.code,{children:"MonadTime"})," and ",(0,o.jsx)(s.code,{children:"MonadTimer"})]}),"\n",(0,o.jsxs)(s.li,{children:["Concurrency through ",(0,o.jsx)(s.code,{children:"MonadAsync"}),", ",(0,o.jsx)(s.code,{children:"MonadFork"})]}),"\n",(0,o.jsxs)(s.li,{children:["Exceptions through ",(0,o.jsx)(s.code,{children:"MonadThrow"}),", ",(0,o.jsx)(s.code,{children:"MonadCatch"})," and ",(0,o.jsx)(s.code,{children:"MonadMask"})]}),"\n"]}),"\n",(0,o.jsx)(s.h2,{id:"consequences",children:"Consequences"}),"\n",(0,o.jsxs)(s.p,{children:["We can use ",(0,o.jsx)(s.code,{children:"io-sim"})," to evaluate IO-ish functions easily"]}),"\n",(0,o.jsxs)(s.p,{children:["Instantiation to concrete IO is pushed at the outermost layer, eg. in the ",(0,o.jsx)(s.code,{children:"Main"})," or tests."]}),"\n",(0,o.jsxs)(s.p,{children:["As some of these functions and typeclasses clash with the\n",(0,o.jsx)(s.a,{href:"https://github.com/input-output-hk/cardano-prelude",children:"cardano-prelude"})," we might\nwant to define a custom prelude (candidate for another ADR)"]})]})}function h(e={}){const{wrapper:s}={...(0,r.R)(),...e.components};return s?(0,o.jsx)(s,{...e,children:(0,o.jsx)(l,{...e})}):l(e)}},28453:(e,s,n)=>{n.d(s,{R:()=>a,x:()=>c});var t=n(96540);const o={},r=t.createContext(o);function a(e){const s=t.useContext(r);return t.useMemo((function(){return"function"==typeof e?e(s):{...s,...e}}),[s,e])}function c(e){let s;return s=e.disableParentContext?"function"==typeof e.components?e.components(o):e.components||o:a(e.components),t.createElement(r.Provider,{value:s},e.children)}},24489:e=>{e.exports=JSON.parse('{"permalink":"/head-protocol/adr/5","source":"@site/adr/2021-06-09_005-use-io-sim-classes.md","title":"5. Use io-classes\\n","description":"Status","date":"2021-06-09T00:00:00.000Z","tags":[{"inline":true,"label":"Accepted","permalink":"/head-protocol/adr/tags/accepted"}],"readingTime":1.055,"hasTruncateMarker":false,"authors":[],"frontMatter":{"slug":"5","title":"5. Use io-classes\\n","authors":[],"tags":["Accepted"]},"unlisted":false,"prevItem":{"title":"4. Use Handle to model Effects\\n","permalink":"/head-protocol/adr/4"},"nextItem":{"title":"6. Network broadcasts all messages\\n","permalink":"/head-protocol/adr/6"}}')}}]);