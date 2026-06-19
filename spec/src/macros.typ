// Hydra macros, ported from macros.tex.
//
// Symbols are `#let` bindings of math content; use them inside math, e.g.
// `$tyNatural -> tyBool$`. Parameterised macros are functions, e.g.
// `$msSign(p, sk, m)$`. Projector superscripts are content meant to be attached
// with `^`, e.g. `$hatmU^hpProjT$`.
//
// Pure-LaTeX layout/algorithm/environment macros (\SetKw*, algobox, walgo,
// sitemize, etc.) are NOT ported here; they belong to the algorithm phase.

// === Variables needed early (defined before first use) ===
#let pu = $sans("ver")$ // public/verification
#let pr = $sans("sig")$ // private/signing

// === Misc ===
#let redtext(x) = text(fill: rgb("#cc0000"))[#x]
#let bluetext(x) = text(fill: blue)[#x]

#let fst(x) = $#x^("st")$
#let snd(x) = $#x^("nd")$
#let trd(x) = $#x^("rd")$
#let ith(x) = $#x^("th")$

#let eps = $epsilon.alt$
#let mc(x) = $cal(#x)$

#let argmax = $op("arg max", limits: #true)$
#let argmin = $op("arg min", limits: #true)$

// === General ===
#let ol(x) = $overline(#x)$

#let mtrue = $mono("true")$
#let mfalse = $mono("false")$

#let spara = $k$
#let nop = $n$
#let party = $sans(p)$
#let parties = $cal(P)$

#let adv = $cal(A)$
#let att = adv
#let advLive = $cal(A)_sans(L)$

#let propName(x) = smallcaps(x)

#let hout(i) = $h_(sans("out"), #i)$
#let hrest = $h_sans("rest")$

#let es = eps

// === Transactions ===
#let adaO = $upright("₳")_sans(o)$

#let tyBool = $bb(B)$
#let tyNatural = $bb(N)$
#let tyInteger = $bb(Z)$
#let tyData = $sans("Data")$
#let tyBytes = $bb(H)$

#let datum = $delta$
#let redeemer = $rho$

#let txContext = $gamma$
#let tyContext = $Gamma$

#let val = $sans("val")$
#let tyValue = $sans("Val")$

#let tx = $upright("tx")$
#let txA = $upright("txA")$
#let txB = $upright("txB")$
#let validTx = $sans("valid-tx")$
#let applytx = $compose$
#let Reach = $sans("Reach")$

#let txTx = $italic("tx")$
#let txTxTy = $italic("Tx")$
#let txCIdTy = $italic("CId")$
#let txTokenTy = $italic("Token")$
#let txKeys = $kappa$
#let tyKeys = $cal(K)^*$
#let txOutRef = $phi$
#let tyOutRef = $Phi$
#let txInputs = $cal(I)$
#let tyInputs = $cal(I)^*$
#let txOutputs = $cal(O)$
#let tyOutputs = $cal(O)^*$
#let txMint = $sans("mint")$
#let ID = $sans("ID")$
#let txIdx = $italic("txIdx")$
#let txValidityMin = $t_sans("min")$
#let txValidityMax = $t_sans("max")$
#let tyValidity = $cal(S)^(arrow.l.r)$

// times and periods
#let Tcontest = $T_sans("contest")$
#let tfinal = $t_sans("final")$
#let Tdeposit = $T_sans("deposit")$

#let txIpend = $i_sans("pend")$
#let txIpendSet = $I_sans("pend")$

#let Tset = $T$
#let Uset = $U$
#let Uinit = $Uset_0$
#let Ufinal = $Uset_sans("final")$

#let recordUTxO = $sans("recordUTxO")$

// === Multisignatures ===
#let ms = $sans("MS")$

#let msSetup = $sans("MS-Setup")$
#let msKeyGen = $sans("MS-KG")$
#let msSign = $sans("MS-Sign")$
#let msVfy = $sans("MS-Verify")$
#let msComb = $sans("MS-ASig")$
#let msCombVK = $sans("MS-AVK")$
#let msCombVfy = $sans("MS-AVerify")$

#let msParams = $Pi$
#let msSig = $sigma$
#let msSigL = $overline(sigma)$
#let msCSig = $tilde(sigma)$
#let msVK = $k^("ver")$
#let msCVK = $tilde(k)$
#let msVKL = $overline(k)$
#let msSK = $k^("sig")$
#let msMsg = $m$

#let initial(x) = $dot(#x)$
#let sVK = $k_pu$
#let sVKI(i) = $k_(#i, pu)$
#let sVKII(i) = $initial(k)_(#i, pu)$
#let sSK = $k_pr$
#let sSKI(i) = $k_(#i, pr)$
#let sSKII(i) = $initial(k)_(#i, pr)$

// === State Machines ===
#let cemS = $S_(#smallcaps("cem"))$
#let cemI = $I_(#smallcaps("cem"))$
#let cemIn = $i_(#smallcaps("cem"))$
#let cemOut = $o_(#smallcaps("cem"))$
#let cemValidator = $nu_(#smallcaps("cem"))$
#let cemDatum = $datum_(#smallcaps("cem"))$
#let cemRedeemer = $redeemer_(#smallcaps("cem"))$
#let cemFinal = $italic("final")_(#smallcaps("cem"))$
#let cemStep = $italic("step")_(#smallcaps("cem"))$
#let cemStepRel(a, b, c, d) = $#a stretch(->)^(#b) (#c, #d)$
#let cemTxCon = $tx^(equiv)$

#let cid = $sans("cid")$ // head id / currency id
#let did = $sans("did")$ // deposit id

#let st = $sans("ST")$
#let pt = $sans("PT")$
#let dt = $sans("DT")$

// == Transactions (named) ==
#let mtxInit = $italic("init")$
#let mtxDeposit = $italic("deposit")$
#let mtxRecover = $italic("recover")$
#let mtxIncrement = $italic("increment")$
#let mtxDecrement = $italic("decrement")$
#let mtxClose = $italic("close")$
#let mtxContest = $italic("contest")$
#let mtxFinalize = $italic("finalize")$
#let mtxFanout = $italic("fanout")$
#let mtxPartialFanout = $italic("partialFanout")$
#let mtxFinalPartialFanout = $italic("finalPartialFanout")$

// == States ==
#let stOpen = $sans("open")$
#let stClosed = $sans("closed")$
#let stSnap = $sans("newestSN")$
#let stFinal = $sans("final")$
#let stFanoutProgress = $sans("fanoutProgress")$

#let seed = $txOutRef_sans("seed")$
#let hMT = $h_sans("MT")$
#let piMT = $pi_sans("MT")$
#let contesters = $cal(C)$

// == Inputs ==
#let stClose = $sans("close")$
#let stContest = $sans("contest")$
#let stFanout = $sans("fanout")$
#let stPartialFanout = $sans("partialFanout")$
#let stFinalPartialFanout = $sans("finalPartialFanout")$

// == Validators, values, datums, redeemers ==
#let muHead = $mu_sans("head")$
#let nuHead = $nu_sans("head")$
#let valHead = $val_sans("head")$
#let datumHead = $datum_sans("head")$
#let redeemerHead = $redeemer_sans("head")$

#let nuDeposit = $nu_sans("deposit")$
#let valDeposit = $val_sans("deposit")$
#let datumDeposit = $datum_sans("deposit")$
#let redeemerDeposit = $redeemer_sans("deposit")$

#let nuSnap = $nu_sans("SN")$
#let nuNewname = $nu_sans("newestSN")$
#let nuHang = $nu_sans("HT")$
#let nuFinal = $nu_sans("final")$

// === Head Protocol ===
// == Algorithms ==
#let HP = $sans("HP")$
#let hpSetup = $sans("Setup")$
#let hpKG = $sans("KeyGen")$
#let hpAgg = $sans("Agg")$
#let hpProt = $sans("Prot")$

// == Protocol ==
#let hpInit = $mono("init")$
#let hpNew = $mono("newTx")$
#let hpSeen = $mono("seen")$
#let hpConf = $mono("conf")$
#let hpSnap = $mono("snap")$
#let hpClose = $mono("close")$
#let hpCont = $mono("cont")$
#let hpFO = $mono("fanOut")$
#let hpRD = $mono("reqDec")$

// == Variables ==
#let hpParams = $Sigma$
#let hpPu = $K_pu$
#let hpPuv = $underline(K)_pu$
#let hpPui(i) = $K_(pu, #i)$
#let hpPr = $K_pr$
#let hpPri(i) = $K_(pr, #i)$

#let hydraKey = $k_sans("H")$
#let hydraKeys = $underline(k)_sans("H")$
#let hydraKeysAgg = $tilde(k)_sans("H")$
#let hydraSigningKey = $msSK_sans("H")$
#let cardanoKey = $k_sans("C")$
#let cardanoKeys = $underline(k)_sans("C")$
#let cardanoSigningKey = $msSK_sans("C")$
#let keyHash = $k^(\#)$

#let hpPuvInit = $initial(underline(K))_pu$

#let hydraKeysAggchain = $hydraKeysAgg^sans("chain")$
#let hydraKeysAgginit = $hydraKeysAgg^sans("setup")$

#let daPuII(i) = $initial(K)_(#i, pu)$
#let daPrII(i) = $initial(K)_(#i, pr)$

#let cardanoKeysinit = $initial(underline(k))_pu$

#let hatv = $hat(v)$
#let barv = $macron(v)$
#let hats = $hat(s)$
#let bars = $macron(s)$
#let barsigma = $macron(sigma)$
#let hatSigma = $hat(Sigma)$
#let hatmU = $hat(cal(U))$
#let barmU = $macron(cal(U))$
#let mL = $cal(L)$
#let hatmL = $hat(cal(L))$
#let barmL = $macron(cal(L))$
#let mT = $cal(T)$
#let hatmT = $hat(cal(T))$
#let hatmDT = $Delta hat(cal(T))$
#let hatmR = $hat(cal(R))$
#let mH = $cal(H)$

#let TR = $T_sans("R")$
#let tTR = $tilde(T)_sans("R")$
#let tR = $tilde(R)$

#let hpSigs = $S$
#let txo = $sans("tx")$

// == Commands ==
#let hpRG = $mono("req")$
#let hpAG = $mono("ack")$
#let hpCG = $mono("conf")$

#let hpRT = $mono("reqTx")$
#let hpAT = $mono("ackTx")$
#let hpCT = $mono("confTx")$

#let hpNS = $mono("newSn")$
#let hpRS = $mono("reqSn")$
#let hpRI = $mono("reqInc")$
#let hpAS = $mono("ackSn")$
#let hpCS = $mono("confSn")$

// == Functions ==
#let hash = $sans("hash")$
#let bytes = $sans("bytes")$
#let concat = $sans("concat")$
#let sortOn = $sans("sortOn")$
#let combine = $sans("combine")$

// KZG accumulator functions for partial fanout
#let accSetup = $sans("accSetup")$
#let accCommit = $sans("accCommit")$
#let accCombine = $sans("accCombine")$
#let accWitness = $sans("accWitness")$
#let accVerify = $sans("accVerify")$
#let accExclude = $sans("accExclude")$
#let accVerifyExclude = $sans("accVerifyExclude")$
#let accUTxO = $sans("accUTxO")$
#let accPartial = $sans("accPartial")$
#let accVerifyPartial = $sans("accVerifyPartial")$

#let Txo = $sans("txObj")$
#let Sno = $sans("snObj")$
#let ApplyMax = $sans("uApplyMax")$

#let hpLdr = $sans("leader")$
#let hpMT = $sans("maxTxos")$

#let conf = $sans("conflict")$
#let confTx = $sans("conflict-tx")$

// == Projectors (attach with ^, e.g. base^hpProjT) ==
#let hpProj = $arrow.b (tx, msCSig)$
#let hpProjT = $arrow.b (tx)$
#let hpProjH = $arrow.b (hash)$
#let hpProjSig = $arrow.b (msCSig)$
#let hpProjHs = $arrow.b (hash, msCSig)$
#let hpProjSo = $arrow.b (s, cal(O))$
#let hpProjSos = $arrow.b (s, cal(O), msCSig)$

// == Security ==
#let Ttilde = $tilde(S)$
#let That = $hat(S)$
#let Tbar = $macron(C)$
#let Snapbar = $macron(Sigma)$

#let TxNewAll = $cal(N)$

#let Hcont = $H_sans("cont")$
#let honest = $cal(H)$
#let contSet = $cal(C)$
#let Cchain = $C_sans("chain")$
#let USN(i) = $upright("SN")_(#i)$
#let setSN(i) = $tilde(T)_(#i)$
#let curSN(i) = $upright("SN")_(sans("cur"), #i)$

#let INV(i) = $sans("INV")_(#i)$
#let atti(i) = $""^((#i))$

// === Mediator Protocol ===
#let gcClientNewHead = $mono("clientNewHead")$
#let gcClientTx = $mono("clientTx")$
#let gcClientClose = $mono("closeTx")$
#let gcChainInitial = $mono("initialTx")$
#let gcChainClose = $mono("closeTx")$
#let gcChainContest = $mono("contestTx")$
#let gcChainFanout = $mono("fanoutTx")$
#let gcChainClosedTO = $mono("chainClosedTimeOut")$

#let gcChainRef = $sans("chain")$
#let gcClientRef = $sans("client")$
#let gcHeadRef = $sans("head")$
#let gcChainPost = $sans("postTx")$
#let gcUTXOset = $sans("UTxOs")$

// === On-Chain Verification ===
#let ocvInitial = $sans("Initial")$
#let ocvFinalize = $sans("Finalize")$
#let ocvClose = $sans("Close")$
#let ocvContest = $sans("Contest")$
#let ocvFinal = $sans("Final")$
#let ocvIncrement = $sans("Increment")$
#let ocvDecrement = $sans("Decrement")$
#let ocvSnapshot = $sans("Snapshot")$
#let ocvValidSnap = $sans("ValidSN")$
#let ocvValidHang = $sans("ValidHT")$
#let ocvClaim = $sans("Claim")$
#let ocvAllocate = $sans("Allocate")$
#let ocvFanout = $sans("Fanout")$

#let hInit = $hash_sans("init")$
#let imax = $i_sans("max")$
#let symFinal = $mono("final")$

#let applicable = $sans("applicable")$

// === DA Game variables ===
#let daPID = $sans("ID")$
#let daGlobal = $Sigma$
#let daPu = $K_pu$
#let daPuV = $macron(K)_pu$
#let daPuI(i) = $K_(#i, pu)$
#let daPr = $K_pr$
#let daPrI(i) = $K_(#i, pr)$

#let daCInit = $mono("init")$
#let daCNew = $mono("new")$
#let daCSeen = $mono("seen")$
#let daCConf = $mono("conf")$
#let daCCert = $mono("cert")$
#let daCComp = $mono("comp")$

#let Store = $sans("store")$
#let Sign = $sans("sign")$
#let Combine = $sans("sigCombine")$
#let Verify = $sans("SigVerify")$
#let Complete = $sans("Complete")$

#let hyPu = msCVK
#let hyPr = msSK

// === Merkle-Patricia Trees ===
#let MPTalph = $A$

#let MPTInit = $sans("MPT-Init")$
#let MPTHash = $sans("MPT-Hash")$
#let MPTMemb = $sans("MPT-Memb")$
#let MPTBuild = $sans("MPT-Build")$
#let MPTPath = $sans("MPT-Path")$

#let MPTverMemb = $sans("MPT-VfyMemb")$
#let MPTcompRA = $sans("MPT-CompRA")$
#let MPTcompSpl = $sans("MPT-CompSpl")$

#let CP = $sans("CP")$
#let RP = $sans("RP")$
#let Proj = $sans("Proj")$
#let Sum = $sans("Sum")$
#let Size = $sans("Size")$

#let MPTroot = $h_sans("root")$
#let MPTnodes = $N$
#let MPTpre = $sans("pre")$
#let MPTnode = $sans("node")$
#let MPTleaf = $sans("leaf")$
#let MPTkey = $k$
#let MPTvalue = $v$
#let MPTaux = $sans("aux")$
#let MPTsplit = $sans("split")$

// === MF ===
#let mf(x) = text(fill: red)[#x]
#let symdif = $union.dot$
#let defeq = $eq.delta$
#let sigmaterial = $Phi$

#let dparagraph(x) = [#v(0.5em) #strong(x)]
