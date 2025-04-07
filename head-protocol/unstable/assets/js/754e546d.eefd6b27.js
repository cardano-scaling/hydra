"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[4527],{79294:(e,n,s)=>{s.r(n),s.d(n,{assets:()=>d,contentTitle:()=>a,default:()=>u,frontMatter:()=>i,metadata:()=>t,toc:()=>l});var t=s(82470),r=s(74848),o=s(28453);const i={slug:17,title:"17. Use UDP protocol for Hydra networking\n",authors:[],tags:["Superseded"]},a=void 0,d={authorsImageUrls:[]},l=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Decision",id:"decision",level:2},{value:"Details",id:"details",level:2},{value:"Consequences",id:"consequences",level:2}];function c(e){const n={a:"a",em:"em",h2:"h2",li:"li",ol:"ol",p:"p",ul:"ul",...(0,o.R)(),...e.components};return(0,r.jsxs)(r.Fragment,{children:[(0,r.jsx)(n.h2,{id:"status",children:"Status"}),"\n",(0,r.jsxs)(n.p,{children:["Superseded (as never implemented) by ",(0,r.jsx)(n.a,{href:"/adr/32",children:"ADR 32"})]}),"\n",(0,r.jsx)(n.h2,{id:"context",children:"Context"}),"\n",(0,r.jsxs)(n.p,{children:["Current Hydra networking layer is based on ",(0,r.jsx)(n.a,{href:"https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-network-framework",children:"Ouroboros network framework"})," networking stack which, among other features, provides:"]}),"\n",(0,r.jsxs)(n.ol,{children:["\n",(0,r.jsxs)(n.li,{children:["An abstraction of stream-based duplex communication channels called a ",(0,r.jsx)(n.a,{href:"https://github.com/input-output-hk/ouroboros-network/blob/6c15a8093bac34091ad96af2b8b0d1f7fe54b732/ouroboros-network-framework/src/Ouroboros/Network/Snocket.hs",children:"Snocket"}),","]}),"\n",(0,r.jsx)(n.li,{children:"A Multiplexing connection manager that manages a set of equivalent peers, maintains connectivity, and ensures diffusion of messages to/from all peers,"}),"\n",(0,r.jsxs)(n.li,{children:["Typed protocols for expressing the logic of message exchanges as a form of ",(0,r.jsx)(n.em,{children:"state machine"}),"."]}),"\n"]}),"\n",(0,r.jsxs)(n.p,{children:["While it's been working mostly fine so far, the abstractions and facilities provided by this network layer are not well suited for Hydra Head networking. Some of the questions and shortcomings are discussed in a document on ",(0,r.jsx)(n.a,{href:"/docs/dev/architecture/networking",children:"Networking Requirements"}),", and as the Hydra Head matures it seems time is ripe for overhauling current network implementation to better suite current and future Hydra Head networks needs."]}),"\n",(0,r.jsx)(n.h2,{id:"decision",children:"Decision"}),"\n",(0,r.jsxs)(n.ul,{children:["\n",(0,r.jsxs)(n.li,{children:["Hydra Head nodes communicate by sending messages to other nodes using ",(0,r.jsx)(n.a,{href:"https://en.wikipedia.org/wiki/User_Datagram_Protocol",children:"UDP"})," protocol"]}),"\n"]}),"\n",(0,r.jsx)(n.h2,{id:"details",children:"Details"}),"\n",(0,r.jsxs)(n.ul,{children:["\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.em,{children:"How do nodes know each other?"}),": This is unspecified by this ADR and left for future work, it is assumed that a Hydra node operator knows the IP",":Port"," address of its peers before opening a Head with them"]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.em,{children:"Are messages encrypted?"}),": This should probably be the case in order to ensure Heads' privacy but is also left for future work"]}),"\n",(0,r.jsxs)(n.li,{children:[(0,r.jsx)(n.em,{children:"How are nodes identified?"}),": At the moment they are identified by their IP",":Port"," pair. As we implement more of the setup process from section 4 of the Hydra Head paper, we should identify nodes by some public key(hash) and resolve the actual IP",":Port"," pair using some other mechanism"]}),"\n"]}),"\n",(0,r.jsx)(n.h2,{id:"consequences",children:"Consequences"}),"\n",(0,r.jsxs)(n.ul,{children:["\n",(0,r.jsxs)(n.li,{children:["Node's ",(0,r.jsx)(n.em,{children:"HeadLogic"})," handles lost, duplicates, and out-of-order messages using ",(0,r.jsx)(n.em,{children:"retry"})," and ",(0,r.jsx)(n.em,{children:"timeout"})," mechanisms"]}),"\n",(0,r.jsx)(n.li,{children:"Messages should carry a unique identifier, eg. source node and index"}),"\n",(0,r.jsx)(n.li,{children:"Protocol, eg. messages format, is documented"}),"\n"]})]})}function u(e={}){const{wrapper:n}={...(0,o.R)(),...e.components};return n?(0,r.jsx)(n,{...e,children:(0,r.jsx)(c,{...e})}):c(e)}},28453:(e,n,s)=>{s.d(n,{R:()=>i,x:()=>a});var t=s(96540);const r={},o=t.createContext(r);function i(e){const n=t.useContext(o);return t.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function a(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(r):e.components||r:i(e.components),t.createElement(o.Provider,{value:n},e.children)}},82470:e=>{e.exports=JSON.parse('{"permalink":"/head-protocol/unstable/adr/17","source":"@site/adr/2022-03-28_017-udp-networking.md","title":"17. Use UDP protocol for Hydra networking\\n","description":"Status","date":"2022-03-28T00:00:00.000Z","tags":[{"inline":true,"label":"Superseded","permalink":"/head-protocol/unstable/adr/tags/superseded"}],"readingTime":1.53,"hasTruncateMarker":false,"authors":[],"frontMatter":{"slug":"17","title":"17. Use UDP protocol for Hydra networking\\n","authors":[],"tags":["Superseded"]},"unlisted":false,"prevItem":{"title":"16. Keep Rejected ADRs\\n","permalink":"/head-protocol/unstable/adr/16"},"nextItem":{"title":"18. Single state in Hydra.Node.\\n","permalink":"/head-protocol/unstable/adr/18"}}')}}]);