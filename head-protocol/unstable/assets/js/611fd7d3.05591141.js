"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[6161],{34687:(e,a,t)=>{t.r(a),t.d(a,{assets:()=>d,contentTitle:()=>i,default:()=>l,frontMatter:()=>r,metadata:()=>n,toc:()=>c});const n=JSON.parse('{"id":"basic/index","title":"Basic Hydra head","description":"This document is a work in progress.","source":"@site/topologies/basic/index.md","sourceDirName":"basic","slug":"/basic/","permalink":"/head-protocol/unstable/topologies/basic/","draft":false,"unlisted":false,"editUrl":"https://github.com/cardano-scaling/hydra/tree/master/docs/topologies/basic/index.md","tags":[],"version":"current","sidebarPosition":2,"frontMatter":{"sidebar_label":"Basic Hydra head","sidebar_position":2},"sidebar":"defaultSidebar","previous":{"title":"Topologies","permalink":"/head-protocol/unstable/topologies/"},"next":{"title":"Managed Hydra head","permalink":"/head-protocol/unstable/topologies/managed/"}}');var s=t(74848),o=t(28453);const r={sidebar_label:"Basic Hydra head",sidebar_position:2},i="Basic Hydra head",d={},c=[];function h(e){const a={admonition:"admonition",code:"code",em:"em",h1:"h1",header:"header",p:"p",...(0,o.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(a.header,{children:(0,s.jsx)(a.h1,{id:"basic-hydra-head",children:"Basic Hydra head"})}),"\n",(0,s.jsx)(a.admonition,{type:"note",children:(0,s.jsxs)(a.p,{children:["\ud83d\udee0"," This document is a work in progress."]})}),"\n",(0,s.jsx)(a.p,{children:"This document outlines the deployment architecture of a basic Hydra head. It serves as a foundational reference for other topologies discussed in this chapter and is illustrated below:"}),"\n",(0,s.jsx)("p",{align:"center",children:(0,s.jsx)("img",{src:t(19638).A,alt:"Basic Hydra Head",height:400})}),"\n",(0,s.jsxs)(a.p,{children:["The basic setup of a Hydra head involves several ",(0,s.jsx)(a.code,{children:"hydra-node"}),"s, each connected to the Cardano network via a ",(0,s.jsx)(a.code,{children:"cardano-node"})," (not depicted in the diagram). A Hydra client, such as ",(0,s.jsx)(a.code,{children:"hydra-tui"}),", typically connects locally to a ",(0,s.jsx)(a.code,{children:"hydra-node"})," to initiate a Hydra head using an off-chain network. The diagram displays two Hydra heads (colored blue and green) established between two distinct sets of ",(0,s.jsx)(a.code,{children:"hydra-node"}),"s. The lines in the diagram represent Hydra network connections, and the circles symbolize the Hydra head state and credentials, collectively referred to as a ",(0,s.jsx)(a.em,{children:"Hydra head party"}),"."]}),"\n",(0,s.jsxs)(a.p,{children:["The diagram does not show multiple, logical Hydra heads operating concurrently within the same ",(0,s.jsx)(a.code,{children:"hydra-node"}),". This capability, likely to be supported in the future, would facilitate the reuse of network connections between ",(0,s.jsx)(a.code,{children:"hydra-node"})," processes."]}),"\n",(0,s.jsxs)(a.p,{children:["Each head, whether blue or green, progresses independently and requires the endorsement of all respective ",(0,s.jsx)(a.em,{children:"Hydra parties"})," within each head. For instance, the green head requires two signatures, while the blue head requires four."]})]})}function l(e={}){const{wrapper:a}={...(0,o.R)(),...e.components};return a?(0,s.jsx)(a,{...e,children:(0,s.jsx)(h,{...e})}):h(e)}},19638:(e,a,t)=>{t.d(a,{A:()=>n});const n=t.p+"assets/images/basic-hydra-head-31e4fded6123899b955e2ec45ad63e78.jpg"},28453:(e,a,t)=>{t.d(a,{R:()=>r,x:()=>i});var n=t(96540);const s={},o=n.createContext(s);function r(e){const a=n.useContext(o);return n.useMemo((function(){return"function"==typeof e?e(a):{...a,...e}}),[a,e])}function i(e){let a;return a=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:r(e.components),n.createElement(o.Provider,{value:a},e.children)}}}]);