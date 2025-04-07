"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[5239],{39208:(e,t,o)=>{o.r(t),o.d(t,{assets:()=>d,contentTitle:()=>c,default:()=>h,frontMatter:()=>l,metadata:()=>n,toc:()=>u});const n=JSON.parse('{"id":"index","title":"Topologies","description":"The Hydra Head protocol is a well-defined layer 2 consensus protocol, as detailed in the developer documentation section. However, understanding the protocol does not fully address how to implement it on a larger scale or explore the potential topologies that can be achieved. While the example use cases help elucidate potential applications, the topologies described below offer insights into various ways Hydra nodes and Hydra heads could be deployed and interconnected.","source":"@site/topologies/index.md","sourceDirName":".","slug":"/","permalink":"/head-protocol/unstable/topologies/","draft":false,"unlisted":false,"editUrl":"https://github.com/cardano-scaling/hydra/tree/master/docs/topologies/index.md","tags":[],"version":"current","sidebarPosition":1,"frontMatter":{"sidebar_label":"Topologies","sidebar_position":1},"sidebar":"defaultSidebar","next":{"title":"Basic Hydra head","permalink":"/head-protocol/unstable/topologies/basic/"}}');var s=o(74848),r=o(28453),i=o(41830),a=o(60609);const l={sidebar_label:"Topologies",sidebar_position:1},c="Topologies",d={},u=[];function p(e){const t={a:"a",em:"em",h1:"h1",header:"header",p:"p",...(0,r.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(t.header,{children:(0,s.jsx)(t.h1,{id:"topologies",children:"Topologies"})}),"\n",(0,s.jsxs)(t.p,{children:["The Hydra Head protocol is a well-defined layer 2 consensus protocol, as detailed in the ",(0,s.jsx)(t.a,{href:"/docs/dev",children:"developer documentation section"}),". However, understanding the protocol does not fully address how to implement it on a larger scale or explore the potential ",(0,s.jsx)(t.em,{children:"topologies"})," that can be achieved. While the ",(0,s.jsx)(t.a,{href:"/use-cases",children:"example use cases"})," help elucidate potential applications, the ",(0,s.jsx)(t.em,{children:"topologies"})," described below offer insights into various ways Hydra nodes and Hydra heads could be deployed and interconnected."]}),"\n",(0,s.jsx)(t.p,{children:"As the community grows and more users develop solutions on top of Hydra, the 'catalog' of topologies will expand. This expansion aims to assist newcomers in discovering and constructing the deployment model that best fits their use case."}),"\n","\n",(0,s.jsx)(i.A,{items:(0,a.t)().items.filter((({docId:e})=>"index"!=e))})]})}function h(e={}){const{wrapper:t}={...(0,r.R)(),...e.components};return t?(0,s.jsx)(t,{...e,children:(0,s.jsx)(p,{...e})}):p(e)}},41830:(e,t,o)=>{o.d(t,{A:()=>y});o(96540);var n=o(18215),s=o(26972),r=o(28774),i=o(53465),a=o(16654),l=o(21312),c=o(6028);const d={cardContainer:"cardContainer_fWXF",cardTitle:"cardTitle_rnsV",cardDescription:"cardDescription_PWke"};var u=o(74848);function p(e){let{href:t,children:o}=e;return(0,u.jsx)(r.A,{href:t,className:(0,n.A)("card padding--lg",d.cardContainer),children:o})}function h(e){let{href:t,icon:o,title:s,description:r}=e;return(0,u.jsxs)(p,{href:t,children:[(0,u.jsxs)(c.A,{as:"h2",className:(0,n.A)("text--truncate",d.cardTitle),title:s,children:[o," ",s]}),r&&(0,u.jsx)("p",{className:(0,n.A)("text--truncate",d.cardDescription),title:r,children:r})]})}function m(e){let{item:t}=e;const o=(0,s.Nr)(t),n=function(){const{selectMessage:e}=(0,i.W)();return t=>e(t,(0,l.T)({message:"1 item|{count} items",id:"theme.docs.DocCard.categoryDescription.plurals",description:"The default description for a category card in the generated index about how many items this category includes"},{count:t}))}();return o?(0,u.jsx)(h,{href:o,icon:"\ud83d\uddc3\ufe0f",title:t.label,description:t.description??n(t.items.length)}):null}function f(e){let{item:t}=e;const o=(0,a.A)(t.href)?"\ud83d\udcc4\ufe0f":"\ud83d\udd17",n=(0,s.cC)(t.docId??void 0);return(0,u.jsx)(h,{href:t.href,icon:o,title:t.label,description:t.description??n?.description})}function g(e){let{item:t}=e;switch(t.type){case"link":return(0,u.jsx)(f,{item:t});case"category":return(0,u.jsx)(m,{item:t});default:throw new Error(`unknown item type ${JSON.stringify(t)}`)}}function x(e){let{className:t}=e;const o=(0,s.$S)();return(0,u.jsx)(y,{items:o.items,className:t})}function y(e){const{items:t,className:o}=e;if(!t)return(0,u.jsx)(x,{...e});const r=(0,s.d1)(t);return(0,u.jsx)("section",{className:(0,n.A)("row",o),children:r.map(((e,t)=>(0,u.jsx)("article",{className:"col col--6 margin-bottom--lg",children:(0,u.jsx)(g,{item:e})},t)))})}},53465:(e,t,o)=>{o.d(t,{W:()=>c});var n=o(96540),s=o(44586);const r=["zero","one","two","few","many","other"];function i(e){return r.filter((t=>e.includes(t)))}const a={locale:"en",pluralForms:i(["one","other"]),select:e=>1===e?"one":"other"};function l(){const{i18n:{currentLocale:e}}=(0,s.A)();return(0,n.useMemo)((()=>{try{return function(e){const t=new Intl.PluralRules(e);return{locale:e,pluralForms:i(t.resolvedOptions().pluralCategories),select:e=>t.select(e)}}(e)}catch(t){return console.error(`Failed to use Intl.PluralRules for locale "${e}".\nDocusaurus will fallback to the default (English) implementation.\nError: ${t.message}\n`),a}}),[e])}function c(){const e=l();return{selectMessage:(t,o)=>function(e,t,o){const n=e.split("|");if(1===n.length)return n[0];n.length>o.pluralForms.length&&console.error(`For locale=${o.locale}, a maximum of ${o.pluralForms.length} plural forms are expected (${o.pluralForms.join(",")}), but the message contains ${n.length}: ${e}`);const s=o.select(t),r=o.pluralForms.indexOf(s);return n[Math.min(r,n.length-1)]}(o,t,e)}}},28453:(e,t,o)=>{o.d(t,{R:()=>i,x:()=>a});var n=o(96540);const s={},r=n.createContext(s);function i(e){const t=n.useContext(r);return n.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function a(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:i(e.components),n.createElement(r.Provider,{value:t},e.children)}}}]);