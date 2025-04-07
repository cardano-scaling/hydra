"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[830],{22499:(e,n,r)=>{r.r(n),r.d(n,{assets:()=>l,contentTitle:()=>h,default:()=>u,frontMatter:()=>c,metadata:()=>o,toc:()=>m});const o=JSON.parse('{"id":"how-to/operating-hydra","title":"Operate a Hydra node","description":"This page guides Hydra users on troubleshooting issues when running their instances of hydra-node and participating in a Hydra head.","source":"@site/docs/how-to/operating-hydra.md","sourceDirName":"how-to","slug":"/how-to/operating-hydra","permalink":"/head-protocol/docs/how-to/operating-hydra","draft":false,"unlisted":false,"editUrl":"https://github.com/cardano-scaling/hydra/tree/master/docs/docs/how-to/operating-hydra.md","tags":[],"version":"current","sidebarPosition":4,"frontMatter":{"sidebar_position":4},"sidebar":"userDocumentation","previous":{"title":"Decommit funds","permalink":"/head-protocol/docs/how-to/incremental-decommit"},"next":{"title":"Extend the node with event source and sinks","permalink":"/head-protocol/docs/how-to/event-sinks-and-sources"}}');var i=r(74848),t=r(28453),s=(r(96540),r(33745));const a={terminalWindow:"terminalWindow_wGrl",terminalWindowHeader:"terminalWindowHeader_o9Cs",terminalWindowBody:"terminalWindowBody_tzdS",row:"row_Rn7G",buttons:"buttons_IGLB",right:"right_fWp9",dot:"dot_fGZE"};function d(e){let{children:n,minHeight:r}=e;const o="string"==typeof n?(0,i.jsx)(s.A,{children:n}):n;return(0,i.jsxs)("div",{className:a.terminalWindow,style:{minHeight:r},children:[(0,i.jsx)("div",{className:a.terminalWindowHeader,children:(0,i.jsxs)("div",{className:a.buttons,children:[(0,i.jsx)("span",{className:a.dot,style:{background:"#f25f58"}}),(0,i.jsx)("span",{className:a.dot,style:{background:"#fbbe3c"}}),(0,i.jsx)("span",{className:a.dot,style:{background:"#58cb42"}})]})}),(0,i.jsx)("div",{className:a.terminalWindowBody,children:o})]})}const c={sidebar_position:4},h="Operate a Hydra node",l={},m=[{value:"Example setup",id:"example-setup",level:2},{value:"Google Cloud with Terraform",id:"google-cloud-with-terraform",level:3},{value:"Logs",id:"logs",level:2},{value:"Monitoring",id:"monitoring",level:2},{value:"Common issues",id:"common-issues",level:2},{value:"No head is observed from the chain",id:"no-head-is-observed-from-the-chain",level:3},{value:"Head does not make progress",id:"head-does-not-make-progress",level:3}];function p(e){const n={a:"a",code:"code",em:"em",h1:"h1",h2:"h2",h3:"h3",header:"header",li:"li",p:"p",pre:"pre",ul:"ul",...(0,t.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(n.header,{children:(0,i.jsx)(n.h1,{id:"operate-a-hydra-node",children:"Operate a Hydra node"})}),"\n","\n",(0,i.jsxs)(n.p,{children:["This page guides Hydra users on troubleshooting issues when running their instances of ",(0,i.jsx)(n.code,{children:"hydra-node"})," and participating in a Hydra head."]}),"\n",(0,i.jsx)(n.h2,{id:"example-setup",children:"Example setup"}),"\n",(0,i.jsxs)(n.p,{children:["We offer sample node configurations that will help you get started with hosting a Hydra node on virtual machines in the cloud. These configurations are available in the ",(0,i.jsxs)(n.a,{href:"https://github.com/cardano-scaling/hydra/tree/master/sample-node-config/",children:[(0,i.jsx)(n.code,{children:"sample-node-config/"})," directory"]}),"."]}),"\n",(0,i.jsx)(n.h3,{id:"google-cloud-with-terraform",children:"Google Cloud with Terraform"}),"\n",(0,i.jsxs)(n.p,{children:["This setup includes a ",(0,i.jsx)(n.a,{href:"https://github.com/cardano-scaling/hydra/blob/master/sample-node-config/gcp/docker-compose.yaml",children:"docker-compose.yaml"})," file, which serves as a robust template for configuring ",(0,i.jsx)(n.code,{children:"cardano-node"})," and ",(0,i.jsx)(n.code,{children:"hydra-node"})," services. Also, various scripts are provided to help you set up your cluster."]}),"\n",(0,i.jsx)(n.h2,{id:"logs",children:"Logs"}),"\n",(0,i.jsxs)(n.p,{children:["Following the principles outlined in ",(0,i.jsx)(n.a,{href:"/adr/9",children:"ADR-9"}),", the ",(0,i.jsx)(n.code,{children:"hydra-node"})," emits ",(0,i.jsx)(n.a,{href:"https://json.org",children:"JSON"})," formatted logs to the ",(0,i.jsx)(n.code,{children:"stdout"})," stream, with one log item per line. These log entries conform to a specific ",(0,i.jsx)(n.a,{href:"https://github.com/cardano-scaling/hydra/blob/master/hydra-node/json-schemas/logs.yaml",children:"JSON schema"}),". We deliberately maintain the logging mechanism simple and non-configurable to facilitate the integration of Hydra logs into broader log analysis infrastructures, including custom ELK stacks, third-party services, or Docker sidecars."]}),"\n",(0,i.jsx)(n.h2,{id:"monitoring",children:"Monitoring"}),"\n",(0,i.jsxs)(n.p,{children:["When the ",(0,i.jsx)(n.code,{children:"--monitoring-port PORT"})," argument is provided, the ",(0,i.jsx)(n.code,{children:"hydra-node"})," executable will expose a ",(0,i.jsx)(n.a,{href:"https://prometheus.io",children:"Prometheus"})," compatible HTTP ",(0,i.jsx)(n.code,{children:"/metrics"})," endpoint on the specified port to enable metrics scraping."]}),"\n",(0,i.jsxs)(n.p,{children:["For instance, if a ",(0,i.jsx)(n.code,{children:"hydra-node"})," is initiated with ",(0,i.jsx)(n.code,{children:"--monitoring-port 6001"}),", the following command:"]}),"\n",(0,i.jsx)(d,{children:(0,i.jsxs)(n.p,{children:["curl ",(0,i.jsx)(n.a,{href:"http://localhost:6001/metrics",children:"http://localhost:6001/metrics"})]})}),"\n",(0,i.jsx)(n.p,{children:"will output:"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:'# TYPE hydra_head_confirmed_tx counter\nhydra_head_confirmed_tx  0\n# TYPE hydra_head_inputs counter\nhydra_head_inputs  50467\n# TYPE hydra_head_requested_tx counter\nhydra_head_requested_tx  0\n# TYPE hydra_head_tx_confirmation_time_ms histogram\nhydra_head_tx_confirmation_time_ms_bucket{le="5.0"} 0.0\nhydra_head_tx_confirmation_time_ms_bucket{le="10.0"} 0.0\nhydra_head_tx_confirmation_time_ms_bucket{le="50.0"} 0.0\nhydra_head_tx_confirmation_time_ms_bucket{le="100.0"} 0.0\nhydra_head_tx_confirmation_time_ms_bucket{le="1000.0"} 0.0\nhydra_head_tx_confirmation_time_ms_bucket{le="+Inf"} 0.0\nhydra_head_tx_confirmation_time_ms_sum  0.0\nhydra_head_tx_confirmation_time_ms_count  0\n'})}),"\n",(0,i.jsx)(n.h2,{id:"common-issues",children:"Common issues"}),"\n",(0,i.jsx)(n.h3,{id:"no-head-is-observed-from-the-chain",children:"No head is observed from the chain"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["Ensure the ",(0,i.jsx)(n.code,{children:"hydra-node"})," is connected to a ",(0,i.jsx)(n.code,{children:"cardano-node"})," operating on the correct network. Verify the ",(0,i.jsx)(n.code,{children:"--network"})," command-line argument and the ",(0,i.jsx)(n.code,{children:"cardano-node"})," configuration."]}),"\n",(0,i.jsxs)(n.li,{children:["Remember, the ",(0,i.jsx)(n.code,{children:"hydra-node"})," cannot start if it cannot connect to the ",(0,i.jsx)(n.code,{children:"cardano-node"}),", which might require time as the ",(0,i.jsx)(n.code,{children:"cardano-node"})," must revalidate its database and potentially reconstruct its ledger state upon startup. Its connections are not open until it is fully prepared. If running as a service or a container, ensure that the orchestrator restarts the process when it crashes."]}),"\n",(0,i.jsxs)(n.li,{children:["Check that the ",(0,i.jsx)(n.em,{children:"Scripts"})," transaction identifier is valid. This identifier is provided on the ",(0,i.jsx)(n.a,{href:"https://github.com/cardano-scaling/hydra/releases/tag/0.10.0",children:"release"})," page for the three major networks (",(0,i.jsx)(n.code,{children:"preview"}),", ",(0,i.jsx)(n.code,{children:"pre-production"}),", ",(0,i.jsx)(n.code,{children:"mainnet"}),")."]}),"\n",(0,i.jsxs)(n.li,{children:["Verify that the ",(0,i.jsx)(n.code,{children:"hydra-node"}),"'s ",(0,i.jsx)(n.em,{children:"Cardano signing key"})," is consistent with the ",(0,i.jsx)(n.em,{children:"Verification key"})," from the ",(0,i.jsx)(n.code,{children:"Init"})," transaction. Ensure the ",(0,i.jsx)(n.code,{children:"--cardano-signing-key"})," parameter points to the correct key, and that peers have the accurate ",(0,i.jsx)(n.code,{children:"--cardano-verification-key"})," for your node."]}),"\n",(0,i.jsxs)(n.li,{children:["Confirm that peers' ",(0,i.jsx)(n.em,{children:"Cardano verification keys"})," are accurate. This mirrors the above issue; check parameters on all peers."]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"head-does-not-make-progress",children:"Head does not make progress"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["Confirm peers are properly connected to each other. Verify the ",(0,i.jsx)(n.code,{children:"--peer"})," arguments point to the correct ",(0,i.jsx)(n.code,{children:"host:port"})," for each peer. The ",(0,i.jsx)(n.code,{children:"PeerConnected"})," message should be observed by the client or appear in the logs and be consistent across all peers involved in a head."]}),"\n",(0,i.jsxs)(n.li,{children:["Ensure the ",(0,i.jsx)(n.em,{children:"Hydra signing key"})," for your node or the ",(0,i.jsx)(n.em,{children:"Hydra verification keys"})," for peers match each node's expectations. Verify that ",(0,i.jsx)(n.code,{children:"AckSn"})," messages are received by all parties and that the ",(0,i.jsx)(n.code,{children:"LogicOutcome"})," log contains no errors."]}),"\n"]})]})}function u(e={}){const{wrapper:n}={...(0,t.R)(),...e.components};return n?(0,i.jsx)(n,{...e,children:(0,i.jsx)(p,{...e})}):p(e)}}}]);