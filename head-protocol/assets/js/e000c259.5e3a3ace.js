"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[6637],{72983:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>s,contentTitle:()=>d,default:()=>h,frontMatter:()=>r,metadata:()=>o,toc:()=>l});var o=t(70665),a=t(74848),i=t(28453);const r={slug:28,title:"28. Offline mode\n",authors:["cardenaso11"],tags:["Accepted"]},d=void 0,s={authorsImageUrls:[void 0]},l=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Decision",id:"decision",level:2},{value:"Consequences",id:"consequences",level:2}];function c(e){const n={h2:"h2",p:"p",...(0,i.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(n.h2,{id:"status",children:"Status"}),"\n",(0,a.jsx)(n.p,{children:"Accepted"}),"\n",(0,a.jsx)(n.h2,{id:"context",children:"Context"}),"\n",(0,a.jsx)(n.p,{children:"Currently, the Hydra node requires a Layer 1 Cardano node running in order to operate; The L1 node is needed to submit and watch for L1 transactions. Generally speaking, the transactions watched are for learning the state of the Hydra node, as reflected by the L1 chain. The transactions submitted are to transition between states (e.g. after submitting a Commit tx to the L1, a node watches to see when all other nodes have also Committed.)"}),"\n",(0,a.jsx)(n.p,{children:"There are applications for the Hydra node where interaction with an L1 chain is unnecessary. Offline mode will be a key component of the Gummiworm protocol, a Layer 2 protocol being built by Sundae Labs, which enables actors other than Hydra head participants to validate transactions that occur in the head."}),"\n",(0,a.jsx)(n.p,{children:"The Hydra node offline mode would remove the dependency on the L1 Cardano node, for applications like Gummiworm where it is unneeded. It would also remove the dependency on the L1 Cardano node for peer-to-peer Hydra node communication. This would be useful for other Layer 2s that build on top of Hydra instead of duplicating its efforts, and for anyone who wants to easily validate a set of Cardano transactions."}),"\n",(0,a.jsx)(n.h2,{id:"decision",children:"Decision"}),"\n",(0,a.jsx)(n.p,{children:"Hydra node will be executable in offline mode, as an alternative to the default online mode. When online, the Hydra node depends on querying a Cardano node for Era History information and Genesis parameters. When offline this is not necessary, because the Hydra node will not connect to any Layer 1 ."}),"\n",(0,a.jsx)(n.p,{children:"The initial state of the head will be specified in a flag, which makes any Commit redundant. The flag will specify a file for the starting Layer 2 UTXO. The Hydra node can be configured to write the current UTXO into a file, including the starting UTXO file."}),"\n",(0,a.jsx)(n.p,{children:"A node running in offline mode will not be able to switch between offline and online modes once started, as it is an unlikely use-case that would likely add more complexity."}),"\n",(0,a.jsx)(n.p,{children:"Commit endpoint will return 400 instead of building a transaction, in offline mode."}),"\n",(0,a.jsx)(n.p,{children:"Support for peer Hydra nodes in offline mode is considered out of scope, as it doesn't seem immediately useful. A node running in offline mode will not be configurable with any peer nodes, nor will it make a network connection to any peer nodes."}),"\n",(0,a.jsx)(n.h2,{id:"consequences",children:"Consequences"}),"\n",(0,a.jsx)(n.p,{children:"The Hydra node would be usable offline, for transaction validation, and other custom L2 applications. The lifecycle & state machine associated with a Hydra would remain unchanged in both online, and offline mode."}),"\n",(0,a.jsx)(n.p,{children:"The Hydra node can be deployed and run without an accompanying Cardano node, simplifying deployment and testing."})]})}function h(e={}){const{wrapper:n}={...(0,i.R)(),...e.components};return n?(0,a.jsx)(n,{...e,children:(0,a.jsx)(c,{...e})}):c(e)}},28453:(e,n,t)=>{t.d(n,{R:()=>r,x:()=>d});var o=t(96540);const a={},i=o.createContext(a);function r(e){const n=o.useContext(i);return o.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function d(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:r(e.components),o.createElement(i.Provider,{value:n},e.children)}},70665:e=>{e.exports=JSON.parse('{"permalink":"/head-protocol/adr/28","source":"@site/adr/2023-10-16_028_offline_adr.md","title":"28. Offline mode\\n","description":"Status","date":"2023-10-16T00:00:00.000Z","tags":[{"inline":true,"label":"Accepted","permalink":"/head-protocol/adr/tags/accepted"}],"readingTime":2.22,"hasTruncateMarker":false,"authors":[{"name":"Elaine Cardenas","title":"Software Engineer","url":"https://github.com/cardenaso11","imageURL":"https://github.com/cardenaso11.png","key":"cardenaso11","page":null}],"frontMatter":{"slug":"28","title":"28. Offline mode\\n","authors":["cardenaso11"],"tags":["Accepted"]},"unlisted":false,"prevItem":{"title":"27. Network failures model\\n","permalink":"/head-protocol/adr/27"},"nextItem":{"title":"29. EventSource & EventSink abstractions\\n","permalink":"/head-protocol/adr/29"}}')}}]);