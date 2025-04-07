"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[2825],{79312:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>r,contentTitle:()=>c,default:()=>h,frontMatter:()=>i,metadata:()=>o,toc:()=>d});const o=JSON.parse('{"id":"how-to/submit-transaction","title":"Submit a transaction","description":"This section describes how to submit a transaction to an already open head using the NewTx command of the WebSocket API.","source":"@site/docs/how-to/submit-transaction.md","sourceDirName":"how-to","slug":"/how-to/submit-transaction","permalink":"/head-protocol/docs/how-to/submit-transaction","draft":false,"unlisted":false,"editUrl":"https://github.com/cardano-scaling/hydra/tree/master/docs/docs/how-to/submit-transaction.md","tags":[],"version":"current","sidebarPosition":2,"frontMatter":{"sidebar_position":2},"sidebar":"userDocumentation","previous":{"title":"Commit using a blueprint","permalink":"/head-protocol/docs/how-to/commit-blueprint"},"next":{"title":"Decommit funds","permalink":"/head-protocol/docs/how-to/incremental-decommit"}}');var s=t(74848),a=t(28453);const i={sidebar_position:2},c="Submit a transaction",r={},d=[];function l(e){const n={code:"code",h1:"h1",header:"header",p:"p",pre:"pre",...(0,a.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(n.header,{children:(0,s.jsx)(n.h1,{id:"submit-a-transaction",children:"Submit a transaction"})}),"\n",(0,s.jsxs)(n.p,{children:["This section describes how to submit a transaction to an already open head using the ",(0,s.jsx)(n.code,{children:"NewTx"})," command of the WebSocket API."]}),"\n",(0,s.jsx)(n.p,{children:"First, query the UTXO available in the head:"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{children:"curl localhost:4001/snapshot/utxo | jq\n"})}),"\n",(0,s.jsx)(n.p,{children:"Below is an example response:"}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-json",metastring:'title="Example response of GET /snapshot/utxo"',children:'{\n  "8690d7618bb88825d6ec7cfbe2676779b8f4633cb137a1c12cd31b4c53f90f32#0": {\n    "address": "addr_test1vrdhewmpp96gv6az4vymy80hlw9082sjz6rylt2srpntsdq6njxxu",\n    "datum": null,\n    "datumhash": null,\n    "inlineDatum": null,\n    "referenceScript": null,\n    "value": {\n      "lovelace": 100000000\n    }\n  }\n}\n'})}),"\n",(0,s.jsxs)(n.p,{children:["Assuming the single UTXO is owned by ",(0,s.jsx)(n.code,{children:"some-payment-key.sk"})," and you want to send all of it to another address, you can use ",(0,s.jsx)(n.code,{children:"cardano-cli"})," (or your preferred transaction builder) to construct and sign a transaction:"]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-shell",metastring:'title="Transaction building"',children:"cardano-cli transaction build-raw \\\n  --babbage-era \\\n  --tx-in 8690d7618bb88825d6ec7cfbe2676779b8f4633cb137a1c12cd31b4c53f90f32#0 \\\n  --tx-out addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z+100000000 \\\n  --fee 0 \\\n  --out-file tx.json\n\ncardano-cli transaction sign \\\n  --tx-body-file tx.json \\\n  --signing-key-file some-payment-key.sk \\\n  --out-file tx-signed.json\n\ncat tx-signed.json | jq -c '{tag: \"NewTx\", transaction: .}'\n"})}),"\n",(0,s.jsxs)(n.p,{children:["This command generates a message suitable for submission to the ",(0,s.jsx)(n.code,{children:"hydra-node"})," via a WebSocket connection. If ",(0,s.jsx)(n.code,{children:"hydra-node"})," operates on the default port ",(0,s.jsx)(n.code,{children:"4001"}),", the message can be submitted using ",(0,s.jsx)(n.code,{children:"websocat"}),":"]}),"\n",(0,s.jsx)(n.pre,{children:(0,s.jsx)(n.code,{className:"language-shell",children:'cat tx-signed.json | jq -c \'{tag: "NewTx", transaction: .}\' | websocat "ws://127.0.0.1:4001?history=no"\n'})}),"\n",(0,s.jsxs)(n.p,{children:["The transaction will be validated by all connected ",(0,s.jsx)(n.code,{children:"hydra-node"})," instances. It will result in either a ",(0,s.jsx)(n.code,{children:"TxInvalid"})," message, providing a reason for rejection, or a ",(0,s.jsx)(n.code,{children:"TxValid"})," message followed by a ",(0,s.jsx)(n.code,{children:"SnapshotConfirmed"}),", updating the UTXO available in the head shortly after that."]})]})}function h(e={}){const{wrapper:n}={...(0,a.R)(),...e.components};return n?(0,s.jsx)(n,{...e,children:(0,s.jsx)(l,{...e})}):l(e)}},28453:(e,n,t)=>{t.d(n,{R:()=>i,x:()=>c});var o=t(96540);const s={},a=o.createContext(s);function i(e){const n=o.useContext(a);return o.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function c(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:i(e.components),o.createElement(a.Provider,{value:n},e.children)}}}]);