"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[8047],{59609:(e,n,s)=>{s.r(n),s.d(n,{assets:()=>c,contentTitle:()=>r,default:()=>a,frontMatter:()=>d,metadata:()=>t,toc:()=>o});var t=s(26525),l=s(74848),i=s(28453);const d={slug:25,title:"25. Event-sourced, resource-based API\n",authors:[],tags:["Proposed"]},r=void 0,c={authorsImageUrls:[]},o=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Decision",id:"decision",level:2},{value:"Example resources",id:"example-resources",level:3},{value:"Consequences",id:"consequences",level:2}];function h(e){const n={a:"a",code:"code",em:"em",h2:"h2",h3:"h3",li:"li",p:"p",table:"table",tbody:"tbody",td:"td",th:"th",thead:"thead",tr:"tr",ul:"ul",...(0,i.R)(),...e.components};return(0,l.jsxs)(l.Fragment,{children:[(0,l.jsx)(n.h2,{id:"status",children:"Status"}),"\n",(0,l.jsx)(n.p,{children:"Proposed"}),"\n",(0,l.jsx)(n.h2,{id:"context",children:"Context"}),"\n",(0,l.jsxs)(n.ul,{children:["\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:[(0,l.jsx)(n.a,{href:"/adr/3",children:"ADR-3"})," concluded that a full-duplex communication channels are\ndesirable to interact with a ",(0,l.jsx)(n.em,{children:"reactive"})," system."]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["The Client API communicates several types of messages to clients. Currently\nthis ranges from node-level ",(0,l.jsx)(n.code,{children:"PeerConnected"}),", over head-specific ",(0,l.jsx)(n.code,{children:"HeadIsOpen"}),"\nto messages about transactions like ",(0,l.jsx)(n.code,{children:"TxValid"}),". These messages are all of type\n",(0,l.jsx)(n.code,{children:"StateChanged"}),"."]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsx)(n.p,{children:"Current capabilities of the API:"}),"\n",(0,l.jsxs)(n.ul,{children:["\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["Clients can retrieve the whole history of ",(0,l.jsx)(n.code,{children:"StateChanged"})," messages or\nopt-out using a query parameter - all or nothing."]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["There is a welcome message called ",(0,l.jsx)(n.code,{children:"Greetings"})," which is always sent, that\ncontains the last ",(0,l.jsx)(n.code,{children:"headStatus"}),"."]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["There exists a ",(0,l.jsx)(n.code,{children:"GetUTxO"})," query-like ",(0,l.jsx)(n.code,{children:"ClientInput"}),", which will respond with a\n",(0,l.jsx)(n.code,{children:"GetUTxOResponse"})," containing the confirmed UTxO set in an open head, or (!)\nthe currently committed UTxO set when the head is initializing."]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["While overall ",(0,l.jsx)(n.code,{children:"json"})," encoded, clients can choose choose between ",(0,l.jsx)(n.code,{children:"json"})," or\nbinary (",(0,l.jsx)(n.code,{children:"cbor"}),") output of ",(0,l.jsx)(n.code,{children:"transaction"})," fields in several of these using a\nquery parameter."]}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsx)(n.p,{children:'Many of these features have been added in a "quick and dirty" way, by monkey\npatching the encoded JSON.'}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsx)(n.p,{children:"The current capabalities even do not satisfy all user needs:"}),"\n",(0,l.jsxs)(n.ul,{children:["\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["Need to wade through lots of events to know the latest state (except the\nvery basic ",(0,l.jsx)(n.code,{children:"headStatus"})," from the ",(0,l.jsx)(n.code,{children:"Greetings"}),")."]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["Need to poll ",(0,l.jsx)(n.code,{children:"GetUTxO"})," ",(0,l.jsx)(n.em,{children:"or"})," aggregate confirmed transactions on client side\nto know the latest UTxO set for constructing transactions."]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["Inclusion of the whole UTxO set in the head is not always desirable and\nfiltering by address would be beneficial. (not addressed in this ADR though,\nrelevant discussion\n",(0,l.jsx)(n.a,{href:"https://github.com/cardano-scaling/hydra/discussions/797",children:"#797"}),")"]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["As ",(0,l.jsx)(n.a,{href:"/adr/15",children:"ADR-15"})," also proposes, some clients may not need (or should\nnot have) access to administrative information."]}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["It is often a good idea to separate the responsibilities of Commands and\nQueries (",(0,l.jsx)(n.a,{href:"https://martinfowler.com/bliki/CQRS.html",children:"CQRS"}),"), as well as the model they use."]}),"\n"]}),"\n"]}),"\n",(0,l.jsx)(n.h2,{id:"decision",children:"Decision"}),"\n",(0,l.jsxs)(n.ul,{children:["\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["Drop ",(0,l.jsx)(n.code,{children:"GetUTxO"})," and ",(0,l.jsx)(n.code,{children:"GetUTxOResponse"})," messages as they advocate a\nrequest/response way of querying."]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["Realize that ",(0,l.jsx)(n.code,{children:"ClientInput"})," data is actually a ",(0,l.jsx)(n.code,{children:"ClientCommand"})," (renaming them)\nand that ",(0,l.jsx)(n.code,{children:"ServerOutput"})," are just ",(0,l.jsx)(n.code,{children:"projections"})," of the ",(0,l.jsx)(n.a,{href:"/adr/24",children:"internal event stream\n(see ADR-24)"})," into read ",(0,l.jsx)(n.code,{children:"models"})," on the API layer."]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["Compose a versioned (",(0,l.jsx)(n.code,{children:"/v1"}),") API out of resource ",(0,l.jsx)(n.code,{children:"models"}),", which\ncompartmentalize the domain into topics on the API layer."]}),"\n",(0,l.jsxs)(n.ul,{children:["\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["A resource has a ",(0,l.jsx)(n.code,{children:"model"})," type and the ",(0,l.jsx)(n.em,{children:"latest"})," value is the result of a pure\n",(0,l.jsx)(n.code,{children:"projection"})," folded over the ",(0,l.jsx)(n.code,{children:"StateChanged"})," event stream, i.e. ",(0,l.jsx)(n.code,{children:"project :: model -> StateChanged -> model"}),"."]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsx)(n.p,{children:'Each resource is available at some HTTP path, also called "endpoint":'}),"\n",(0,l.jsxs)(n.ul,{children:["\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:[(0,l.jsx)(n.code,{children:"GET"})," requests must respond with the ",(0,l.jsx)(n.em,{children:"latest"})," state in a single response."]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:[(0,l.jsx)(n.code,{children:"GET"})," requests with ",(0,l.jsx)(n.code,{children:"Upgrade: websocket"})," headers must start a websocket\nconnection, push the ",(0,l.jsx)(n.em,{children:"latest"})," state as first message and any resource\nstate updates after."]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["Other HTTP verbs may be accepted by a resource handler, i.e. to issue\nresource-specific ",(0,l.jsx)(n.em,{children:"commands"}),". Any commands accepted must also be available\nvia the corresponding websocket connection."]}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:[(0,l.jsx)(n.code,{children:"Accept"})," request headers can be used to configure the ",(0,l.jsx)(n.code,{children:"Content-Type"})," of the\nresponse"]}),"\n",(0,l.jsxs)(n.ul,{children:["\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["All resources must provide ",(0,l.jsx)(n.code,{children:"application/json"})," responses"]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsx)(n.p,{children:"Some resources might support more content types (e.g. CBOR-encoded binary)"}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["Query parameters may be used to further configure responses of some\nresources. For example, ",(0,l.jsx)(n.code,{children:"?address=<bech32>"})," could be used to filter UTxO by\nsome address."]}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["Keep the semantics of ",(0,l.jsx)(n.code,{children:"/"}),", which accepts websocket upgrade connections and\nsends direct/raw output of ",(0,l.jsx)(n.code,{children:"ServerOutput"})," events on ",(0,l.jsx)(n.code,{children:"/"}),", while accepting all\n",(0,l.jsx)(n.code,{children:"ClientCommand"})," messages."]}),"\n",(0,l.jsxs)(n.ul,{children:["\n",(0,l.jsxs)(n.li,{children:["Define ",(0,l.jsx)(n.code,{children:"ServerOutput"})," also in terms of the ",(0,l.jsx)(n.code,{children:"StateChanged"})," event stream"]}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,l.jsx)(n.h3,{id:"example-resources",children:"Example resources"}),"\n",(0,l.jsx)(n.p,{children:"Example resource paths + HTTP verbs mapped to existing things to demonstrate the\neffects of the decision points above. The mappings may change and are to be\ndocumented by an API specification instead."}),"\n",(0,l.jsxs)(n.table,{children:[(0,l.jsx)(n.thead,{children:(0,l.jsxs)(n.tr,{children:[(0,l.jsx)(n.th,{style:{textAlign:"left"},children:"Path"}),(0,l.jsx)(n.th,{style:{textAlign:"left"},children:"GET"}),(0,l.jsx)(n.th,{style:{textAlign:"left"},children:"POST"}),(0,l.jsx)(n.th,{children:"PATCH"}),(0,l.jsx)(n.th,{style:{textAlign:"left"},children:"DELETE"})]})}),(0,l.jsxs)(n.tbody,{children:[(0,l.jsxs)(n.tr,{children:[(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"/v1/head/status"})}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"HeadStatus(..)"})}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"}),(0,l.jsx)(n.td,{children:"-"}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"})]}),(0,l.jsxs)(n.tr,{children:[(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"/v1/head/snapshot/utxo"})}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"last confirmed snapshot utxo"}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"}),(0,l.jsx)(n.td,{children:"-"}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"})]}),(0,l.jsxs)(n.tr,{children:[(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"/v1/head/snapshot/transactions"})}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"confirmed snapshot txs"}),(0,l.jsxs)(n.td,{style:{textAlign:"left"},children:[(0,l.jsx)(n.code,{children:"NewTx"})," + responses"]}),(0,l.jsx)(n.td,{children:"-"}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"})]}),(0,l.jsxs)(n.tr,{children:[(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"/v1/head/ledger/utxo"})}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"localUTxO"})}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"}),(0,l.jsx)(n.td,{children:"-"}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"})]}),(0,l.jsxs)(n.tr,{children:[(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"/v1/head/ledger/transactions"})}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"localTxs"})}),(0,l.jsxs)(n.td,{style:{textAlign:"left"},children:[(0,l.jsx)(n.code,{children:"NewTx"})," + responses"]}),(0,l.jsx)(n.td,{children:"-"}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"})]}),(0,l.jsxs)(n.tr,{children:[(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"/v1/head/commit"})}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"Chain{draftCommitTx}"})}),(0,l.jsx)(n.td,{children:"-"}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"})]}),(0,l.jsxs)(n.tr,{children:[(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"/v1/head"})}),(0,l.jsxs)(n.td,{style:{textAlign:"left"},children:["all ",(0,l.jsx)(n.code,{children:"/v1/head/*"})," data"]}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"Init"})}),(0,l.jsx)(n.td,{children:(0,l.jsx)(n.code,{children:"Close"})}),(0,l.jsxs)(n.td,{style:{textAlign:"left"},children:[(0,l.jsx)(n.code,{children:"Fanout"})," / ",(0,l.jsx)(n.code,{children:"Abort"})]})]}),(0,l.jsxs)(n.tr,{children:[(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"/v1/protocol-parameters"})}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"current protocol parameters"}),(0,l.jsx)(n.td,{style:{textAlign:"left"}}),(0,l.jsx)(n.td,{}),(0,l.jsx)(n.td,{style:{textAlign:"left"}})]}),(0,l.jsxs)(n.tr,{children:[(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"/v1/cardano-transaction"})}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"Chain{submitTx}"})}),(0,l.jsx)(n.td,{children:"-"}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"})]}),(0,l.jsxs)(n.tr,{children:[(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"/v1/peers"})}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"a list of peers"}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"}),(0,l.jsx)(n.td,{children:"-"}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"})]}),(0,l.jsxs)(n.tr,{children:[(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"/v1/node-version"})}),(0,l.jsxs)(n.td,{style:{textAlign:"left"},children:["node version as in ",(0,l.jsx)(n.code,{children:"Greetings"})]}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"}),(0,l.jsx)(n.td,{children:"-"}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"})]}),(0,l.jsxs)(n.tr,{children:[(0,l.jsx)(n.td,{style:{textAlign:"left"},children:(0,l.jsx)(n.code,{children:"/v1/"})}),(0,l.jsxs)(n.td,{style:{textAlign:"left"},children:["all ",(0,l.jsx)(n.code,{children:"/v1/*"})," data"]}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"}),(0,l.jsx)(n.td,{children:"-"}),(0,l.jsx)(n.td,{style:{textAlign:"left"},children:"-"})]})]})]}),"\n",(0,l.jsxs)(n.p,{children:["Multiple heads are out of scope now and hence paths are not including a\n",(0,l.jsx)(n.code,{children:"<headId>"})," variable section."]}),"\n",(0,l.jsx)(n.h2,{id:"consequences",children:"Consequences"}),"\n",(0,l.jsxs)(n.ul,{children:["\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsx)(n.p,{children:"Clear separation of what types are used for querying and gets subscribed to by\nclients and we have dedicated types for sending data to clients"}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsx)(n.p,{children:"Changes on the querying side of the API are separated from the business logic."}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsx)(n.p,{children:"Clients do not need to aggregate data that is already available on the server\nside without coupling the API to internal state representation."}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsx)(n.p,{children:"Separation of Head operation and Head usage, e.g. some HTTP endpoints can be\noperated with authentication."}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsx)(n.p,{children:"Clients have a fine-grained control over what to subscribe to and what to\nquery."}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsx)(n.p,{children:"Versioned API allows clients to detect incompatibility easily."}),"\n"]}),"\n",(0,l.jsxs)(n.li,{children:["\n",(0,l.jsxs)(n.p,{children:["Need to rewrite how the ",(0,l.jsx)(n.code,{children:"hydra-tui"})," is implemented."]}),"\n"]}),"\n"]})]})}function a(e={}){const{wrapper:n}={...(0,i.R)(),...e.components};return n?(0,l.jsx)(n,{...e,children:(0,l.jsx)(h,{...e})}):h(e)}},28453:(e,n,s)=>{s.d(n,{R:()=>d,x:()=>r});var t=s(96540);const l={},i=t.createContext(l);function d(e){const n=t.useContext(i);return t.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function r(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(l):e.components||l:d(e.components),t.createElement(i.Provider,{value:n},e.children)}},26525:e=>{e.exports=JSON.parse('{"permalink":"/head-protocol/adr/25","source":"@site/adr/2023-08-18_025-resource-based-api.md","title":"25. Event-sourced, resource-based API\\n","description":"Status","date":"2023-08-18T00:00:00.000Z","tags":[{"inline":true,"label":"Proposed","permalink":"/head-protocol/adr/tags/proposed"}],"readingTime":4.58,"hasTruncateMarker":false,"authors":[],"frontMatter":{"slug":"25","title":"25. Event-sourced, resource-based API\\n","authors":[],"tags":["Proposed"]},"unlisted":false,"prevItem":{"title":"24. Persist state changes incrementally\\n","permalink":"/head-protocol/adr/24"},"nextItem":{"title":"26. Stateless transaction observation & construction\\n","permalink":"/head-protocol/adr/26"}}')}}]);