"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[4095],{66938:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>l,contentTitle:()=>o,default:()=>h,frontMatter:()=>i,metadata:()=>n,toc:()=>c});const n=JSON.parse('{"id":"payments/inter-wallet-payments/index","title":"Inter-wallet payments","description":"Inter-wallet payments is a (semi-)custodial peer-to-peer (P2P) payment service through wallet providers.","source":"@site/use-cases/payments/inter-wallet-payments/index.md","sourceDirName":"payments/inter-wallet-payments","slug":"/payments/inter-wallet-payments/","permalink":"/head-protocol/use-cases/payments/inter-wallet-payments/","draft":false,"unlisted":false,"editUrl":"https://github.com/cardano-scaling/hydra/tree/master/docs/use-cases/payments/inter-wallet-payments/index.md","tags":[],"version":"current","frontMatter":{},"sidebar":"defaultSidebar","previous":{"title":"Payments","permalink":"/head-protocol/use-cases/payments/"},"next":{"title":"Pay-per-use API","permalink":"/head-protocol/use-cases/payments/pay-per-use-api/"}}');var s=a(74848),r=a(28453);const i={},o="Inter-wallet payments",l={},c=[];function d(e){const t={a:"a",admonition:"admonition",h1:"h1",header:"header",p:"p",...(0,r.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(t.header,{children:(0,s.jsx)(t.h1,{id:"inter-wallet-payments",children:"Inter-wallet payments"})}),"\n",(0,s.jsx)(t.p,{children:"Inter-wallet payments is a (semi-)custodial peer-to-peer (P2P) payment service through wallet providers."}),"\n",(0,s.jsx)(t.admonition,{title:"This is a legacy article",type:"caution",children:(0,s.jsx)(t.p,{children:"The payments category will be restructured into a more consistent use-case-centric roadmap of application scenarios."})}),"\n",(0,s.jsxs)(t.p,{children:["A Hydra head has participant limitations, which makes developing a large-scale P2P payment solution complex when each peer is directly involved as a member of the head. While the long-term roadmap includes potential solutions like routing and networks of heads (referenced in ",(0,s.jsx)(t.a,{href:"https://eprint.iacr.org/2021/1188",children:"Interhead Hydra: Two Heads Are Better Than One"})," by Jourenko et al), intermediate solutions may involve trusted parties such as light wallet providers, supplemented by on-chain smart contracts to minimize trust requirements."]}),"\n",(0,s.jsx)(t.p,{children:"In this model, we envision a Hydra head formed by major light wallet providers within the network, all sharing a common interest in offering their users cost-effective and rapid inter-wallet transactions. Each wallet provider acts as a member of the head and processes transactions on behalf of their users. To engage in transactions within the head, users must initially lock the funds they intend to transfer on layer 2 within a specific contract. This contract allows any wallet provider to redeem funds from it, given they possess proof of payment issued by the user."}),"\n",(0,s.jsx)(t.p,{children:"Wallet providers, through their interfaces, display the 'virtual balance' in each account, reflecting the activity on layer 2. The 'effective balance' \u2014 the actual value locked on layer one \u2014 remains unchanged. Users can request any wallet provider to unlock funds from the contract anytime, provided there are remaining funds. This process involves collecting the funds at one or more of these contracts, after which they are redistributed: some are returned to locked accounts, and some are returned to the user who requested the withdrawal."}),"\n",(0,s.jsx)(t.p,{children:"This scenario assumes that traffic between wallet providers is somewhat symmetrical. This means users from one wallet typically spend and receive amounts similar to users from another wallet, maintaining balance within the system. Additionally, wallet providers have the authority to reject payments that would disrupt this balance."})]})}function h(e={}){const{wrapper:t}={...(0,r.R)(),...e.components};return t?(0,s.jsx)(t,{...e,children:(0,s.jsx)(d,{...e})}):d(e)}},28453:(e,t,a)=>{a.d(t,{R:()=>i,x:()=>o});var n=a(96540);const s={},r=n.createContext(s);function i(e){const t=n.useContext(r);return n.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function o(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:i(e.components),n.createElement(r.Provider,{value:t},e.children)}}}]);