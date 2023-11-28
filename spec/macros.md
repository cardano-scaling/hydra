\newcommand\red[1]{\textcolor{red}{\textbf{#1}}}
\newcommand\blue[1]{\textcolor{blue}{\textbf{#1}}}

<!-- Types -->

\newcommand{\tyBool}{\mathbb{B}}
\newcommand{\tyNatural}{\mathbb{N}}
\newcommand{\tyInteger}{\mathbb{Z}}
\newcommand{\tyData}{\mathsf{Data}}
\newcommand{\tyBytes}{\mathbb{H}}
\newcommand{\true}{\mathtt{true}}
\newcommand{\false}{\mathtt{false}}

<!-- Functions -->

\newcommand{\hash}{\mathsf{hash}}
\newcommand{\bytes}{\mathsf{bytes}}
\newcommand{\concat}{\mathsf{concat}}
\newcommand{\sortOn}{\mathsf{sortOn}}
\newcommand{\combine}{\mathsf{combine}}

<!-- Multi-signatures -->

\newcommand{\ms}{\mathsf{MS}}
\newcommand{\msSetup}{\mathsf{MS-Setup}}
\newcommand{\msKeyGen}{\mathsf{MS-KG}}
\newcommand{\msSign}{\mathsf{MS-Sign}}
\newcommand{\msVfy}{\mathsf{MS-Verify}}
\newcommand{\msComb}{\mathsf{MS-ASig}}
\newcommand{\msCombVK}{\mathsf{MS-AVK}}
\newcommand{\msCombVfy}{\mathsf{MS-AVerify}}
\newcommand{\msParams}{\Pi}
\newcommand{\msSig}{\sigma}
\newcommand{\msSigL}{\overline{\sigma}}
\newcommand{\msCSig}{\tilde\sigma}
\newcommand{\msVK}{k^{ver}}
\newcommand{\msCVK}{\tilde{k}}
\newcommand{\msVKL}{\overline{k}}
\newcommand{\msSK}{k^{sig}}
\newcommand{\msMsg}{m}

<!-- EUTxO -->

\newcommand{\val}{\mathsf{val}} <!-- a value -->
\newcommand{\tyValue}{\mathsf{Val}} <!-- type of values -->
\newcommand{\datum}{\delta} <!-- a datum -->
\newcommand{\redeemer}{\rho} <!-- a redeemer -->
\newcommand{\txContext}{\gamma} <!-- validation context -->
\newcommand{\tyContext}{\Gamma} <!-- type of contexts -->
\newcommand{\tx}{\mathit{tx}} <!-- transaction value -->
\newcommand{\txTx}{\mathit{tx}} <!-- TODO: remove -->
\newcommand{\txTxTy}{\mathit{Tx}} <!-- transaction type -->
\newcommand{\txCIdTy}{\mathit{CId}} <!-- currency identifier type -->
\newcommand{\txTokenTy}{\mathit{Token}} <!-- currency token type -->
\newcommand{\txKeys}{\kappa} <!-- (public) keys signing the tx -->
\newcommand{\tyKeys}{\mathcal{K}^*} <!-- type of keys -->
\newcommand{\txOutRef}{\phi} <!-- output reference -->
\newcommand{\tyOutRef}{\Phi} <!-- type of output references -->
\newcommand{\txInputs}{\mathcal{I}} <!-- set of inputs -->
\newcommand{\tyInputs}{\mathcal{I}^*} <!-- type of input sets -->
\newcommand{\txOutputs}{\mathcal{O}} <!-- list of outputs -->
\newcommand{\tyOutputs}{\mathcal{O}^*} <!-- type of output lists -->
\newcommand{\txMint}{\mathsf{mint}} <!-- minted value -->
\newcommand{\txValidityMin}{t_{\mathsf{min}}}
\newcommand{\txValidityMax}{t_{\mathsf{max}}}
\newcommand{\tyValidity}{\mathcal{S}^{\leftrightarrow}} <!-- type of validity intervals -->

<!-- On-chain -->

\newcommand{\stInitial}{\mathsf{initial}}
\newcommand{\stOpen}{\mathsf{open}}
\newcommand{\stClosed}{\mathsf{closed}}
\newcommand{\stSnap}{\mathsf{newestSN}}
\newcommand{\stFinal}{\mathsf{final}}

<!-- Off-chain -->

\newcommand{\Tset}{T}
\newcommand{\Uset}{U}
\newcommand{\Uinit}{\Uset_{0}}
\newcommand{\Ufinal}{\Uset_{\mathsf{final}}}
