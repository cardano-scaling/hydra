"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[5664],{31093:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>c,contentTitle:()=>i,default:()=>h,frontMatter:()=>a,metadata:()=>s,toc:()=>l});var s=n(98880),r=n(74848),o=n(28453);const a={slug:4,title:"4. Use Handle to model Effects\n",authors:[],tags:["Accepted"]},i=void 0,c={authorsImageUrls:[]},l=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Decision",id:"decision",level:2},{value:"Consequences",id:"consequences",level:2}];function d(e){const t={a:"a",code:"code",em:"em",h2:"h2",li:"li",p:"p",pre:"pre",ul:"ul",...(0,o.R)(),...e.components};return(0,r.jsxs)(r.Fragment,{children:[(0,r.jsx)(t.h2,{id:"status",children:"Status"}),"\n",(0,r.jsx)(t.p,{children:"Accepted"}),"\n",(0,r.jsx)(t.h2,{id:"context",children:"Context"}),"\n",(0,r.jsxs)(t.p,{children:["Given we are structuring Hydra node as a ",(0,r.jsx)(t.a,{href:"/adr/2",children:"reactive core"})," we need a way to ensure a strict separation of pure and impure (or effectful) code."]}),"\n",(0,r.jsx)(t.p,{children:"We want to be able to test those impure/effectful parts of the code. This requires a means for exchanging the actual implementation for e.g. the function to send messages over a network."}),"\n",(0,r.jsx)(t.p,{children:"Also we want the ability to swap implementations not only for testing, but also be able\nto accommodate different usage scenarios, e.g. use a different middleware\ndepending on peer configuration."}),"\n",(0,r.jsxs)(t.p,{children:["In Haskell there are various common ",(0,r.jsx)(t.em,{children:"patterns"})," to model effects:"]}),"\n",(0,r.jsxs)(t.ul,{children:["\n",(0,r.jsxs)(t.li,{children:[(0,r.jsx)(t.a,{href:"http://okmij.org/ftp/tagless-final/index.html",children:"Tagless final encoding"})," also known as ",(0,r.jsx)(t.em,{children:"MTL-style"})," although using typeclasses to implement is ",(0,r.jsx)(t.a,{href:"https://www.foxhound.systems/blog/final-tagless/",children:"not necessary"}),", whereby Effect(s) are expressed as typeclass(es) which are propagated as constraints"]}),"\n",(0,r.jsxs)(t.li,{children:[(0,r.jsx)(t.a,{href:"https://reasonablypolymorphic.com/blog/freer-monads/",children:"Free monads"}),", or any variant thereof like Eff, freer, extensible-effects, whereby effect(s) are expressed as ADTs which are ",(0,r.jsx)(t.em,{children:"interpreted"})," in the context of an ",(0,r.jsx)(t.em,{children:"Effect stack"})]}),"\n",(0,r.jsxs)(t.li,{children:[(0,r.jsx)(t.a,{href:"https://jaspervdj.be/posts/2018-03-08-handle-pattern.html",children:"Handle"})," pattern also known as ",(0,r.jsx)(t.em,{children:"record-of-functions"})," whereby effects are grouped together in a datatype with a single record constructor"]}),"\n"]}),"\n",(0,r.jsxs)(t.p,{children:["(These tradeoffs also appear in other functional languages like\n",(0,r.jsx)(t.a,{href:"https://medium.com/@dogwith1eye/prefer-records-of-functions-to-interfaces-d6413af4d2c3",children:"F#"}),")"]}),"\n",(0,r.jsx)(t.p,{children:"There is not one most favored solution though and we all have various\nexperiences with these techniques."}),"\n",(0,r.jsx)(t.h2,{id:"decision",children:"Decision"}),"\n",(0,r.jsxs)(t.p,{children:["Effectful components of the Hydra node (our code) will be defined using the ",(0,r.jsx)(t.em,{children:"Handle pattern"}),"."]}),"\n",(0,r.jsx)(t.p,{children:"There might be other techniques in use because of libraries used etc."}),"\n",(0,r.jsx)(t.h2,{id:"consequences",children:"Consequences"}),"\n",(0,r.jsx)(t.p,{children:"For example, the network component is defined as:"}),"\n",(0,r.jsx)(t.pre,{children:(0,r.jsx)(t.code,{className:"language-hs",children:"newtype Network m = Network\n  { broadcast :: MonadThrow m => HydraMessage -> m ()\n  }\n"})}),"\n",(0,r.jsxs)(t.p,{children:["There might be multiple ",(0,r.jsx)(t.code,{children:"createNetwork :: m (Network m)"})," functions"]})]})}function h(e={}){const{wrapper:t}={...(0,o.R)(),...e.components};return t?(0,r.jsx)(t,{...e,children:(0,r.jsx)(d,{...e})}):d(e)}},28453:(e,t,n)=>{n.d(t,{R:()=>a,x:()=>i});var s=n(96540);const r={},o=s.createContext(r);function a(e){const t=s.useContext(o);return s.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function i(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(r):e.components||r:a(e.components),s.createElement(o.Provider,{value:t},e.children)}},98880:e=>{e.exports=JSON.parse('{"permalink":"/head-protocol/adr/4","source":"@site/adr/2021-06-08_004-use-handle-to-model-effects.md","title":"4. Use Handle to model Effects\\n","description":"Status","date":"2021-06-08T00:00:00.000Z","tags":[{"inline":true,"label":"Accepted","permalink":"/head-protocol/adr/tags/accepted"}],"readingTime":1.355,"hasTruncateMarker":false,"authors":[],"frontMatter":{"slug":"4","title":"4. Use Handle to model Effects\\n","authors":[],"tags":["Accepted"]},"unlisted":false,"prevItem":{"title":"3. Asynchronous Duplex Client API","permalink":"/head-protocol/adr/3"},"nextItem":{"title":"5. Use io-classes\\n","permalink":"/head-protocol/adr/5"}}')}}]);