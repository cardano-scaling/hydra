-- | Clean Haskell surface over the MAlonzo-extracted decidable reference checkers
-- (@spec\/src\/Hydra\/Protocol\/Reference.agda@).
--
-- Every binding below names an export the Agda source fixes with @COMPILE GHC … as …@ (its
-- "extraction surface" section), not a mangled MAlonzo name. MAlonzo appends a definition-order
-- index to the names it mangles, so any additive edit to the Agda renumbers them: binding those
-- would make this shim need hand-editing after unrelated changes, and would let two checkers of the
-- same type be silently swapped. A stable name that disappears is a compile error here instead.
--
-- The @Ops@ records that inject the crypto\/value conjuncts the decidable layer does not model stay
-- on the Agda side; each checker takes the injected decision as a plain function argument, which the
-- differential tests supply as @const True@.
--
-- Domain: every 'Integer' below stands for an Agda ℕ, and callers must pass non-negative values. This
-- is not a soundness caveat that can be ignored; MAlonzo compiles ℕ to 'Integer' with no check, and
-- the two families of arithmetic in the reference fail differently on a negative:
--
-- * the builtin operations accept it silently. @_+_@, @_*_@, @_==_@ and @_\<_@ are the raw 'Integer'
--   operations, and @_-_@ is @max 0 . subtract@, so a negative input yields an answer the ℕ-level
--   proofs in @ReferenceBridge.agda@ never sanctioned.
-- * the structural operations do not terminate. @_==ᵇ_@ and @_≤ᵇ_@ are compiled to a recursion that
--   subtracts one until it matches the literal @0@, which a negative argument never reaches.
--
-- Every field the projections fill is non-negative by construction (token quantities, output counts,
-- list lengths, POSIXTime bounds of a real transaction), so this is a note on the boundary rather
-- than a live defect.
module Hydra.Agda.Reference (
  -- * close
  HsCloseTag (..),
  HsOpen (..),
  HsClosed (..),
  checkClose,

  -- * increment / decrement
  HsIncIO (..),
  checkInc,
  checkDec,
  HsAssetIO (..),
  checkPerAsset,

  -- * contest
  HsContestIO (..),
  checkContest,

  -- * fanout / finalPartialFanout
  HsFanout (..),
  checkFanout,

  -- * deposit recover (νDeposit)
  HsRecoverIO (..),
  checkRecover,

  -- * init (μHead minting policy: token count)
  HsMintIO (..),
  checkInit,

  -- * deposit claim (νDeposit)
  HsClaimIO (..),
  checkClaim,

  -- * participant signature (shared: close / contest / increment / decrement)
  HsSignerIO (..),
  checkParticipantSigned,

  -- * no mint / no burn (shared: close / contest / increment / decrement)
  checkNoMint,

  -- * referenced output is spent (increment claimed deposit / init seed)
  checkRefSpent,

  -- * non-final partial fanout (FanoutProgress → FanoutProgress)
  checkPartialFanout,

  -- * value preservation (shared: close / contest mustPreserveHeadValue)
  checkValuePreserved,

  -- * contest parameter preservation (contest mustNotChangeParameters: headId + contestationPeriod)
  checkContestParams,

  -- * init datum head-id binding (μHead checkDatum: headId == currency)
  checkInitHeadId,

  -- * μHead token burning (Burn redeemer)
  HsBurnIO (..),
  checkBurn,
) where

import MAlonzo.Code.Hydra.Protocol.Reference (
  HsAssetIO (..),
  HsBurnIO (..),
  HsClaimIO (..),
  HsCloseTag (..),
  HsClosed (..),
  HsContestIO (..),
  HsFanout (..),
  HsIncIO (..),
  HsMintIO (..),
  HsOpen (..),
  HsRecoverIO (..),
  HsSignerIO (..),
 )
import MAlonzo.Code.Hydra.Protocol.Reference qualified as M

-- | Extracted decidable close-validity checker. Mirrors the decidable conjuncts of @closeValid@
-- (version\/cp preserved, contesters empty, initial\/any snapshot rules, the recorded contestation
-- deadline @tfinal == validityHi + cp@, and the bounded validity range @hi - lo \<= cp@); proved to
-- reflect them in @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. The two trailing 'Integer'
-- arguments are the tx upper and lower validity bounds (POSIXTime ms). The first argument is the
-- injected crypto\/value\/accumulator decision.
checkClose ::
  (HsOpen -> HsClosed -> HsCloseTag -> Bool) ->
  HsOpen ->
  HsClosed ->
  HsCloseTag ->
  Integer ->
  Integer ->
  Bool
checkClose = M.hsCheckClose

-- | Decidable increment checker: the produced version is @suc@ the input version
-- (@VersionNotIncremented@), the claimed deposit is output 0 of its transaction (@depositIdxI == 0@,
-- the validator's @DepositNotFirstOutput@ guard: the commit digest binds a deposit by transaction id,
-- so sibling outputs would otherwise be interchangeable under one signature) and the head value grows
-- by the deposit on both the lovelace component (@adaIn + adaDelta == adaOut@) and the total non-ada
-- token quantity (@nonAdaIn + nonAdaDelta == nonAdaOut@), the validator's @mustPreserveValue@, which
-- also catches a native-token siphon that an ada-only check would miss. Crypto is injected.
checkInc :: (HsIncIO -> Bool) -> HsIncIO -> Bool
checkInc = M.hsCheckInc

-- | Decidable decrement checker: the produced version is @suc@ the input version, at least one
-- decommit output is materialized (@numDecOutsI >= 1@, counted on @take m (tail outputs)@, which
-- truncates silently; without this an increment snapshot's signature would authorize a decrement
-- that adopts its accumulator while no deposit is spent) and the head value shrinks by the decommit on
-- both the lovelace component (@adaOut + adaDelta == adaIn@) and the total non-ada token quantity
-- (@nonAdaOut + nonAdaDelta == nonAdaIn@): head output + decommitted outputs == head input, the
-- validator's @mustDecreaseValue@. Crypto is injected.
checkDec :: (HsIncIO -> Bool) -> HsIncIO -> Bool
checkDec = M.hsCheckDec

-- | Per-asset value-conservation checker (the finer companion to 'checkInc'\/'checkDec', which check
-- only the @adaOf@\/@nonAdaOf@ totals). Each 'HsAssetIO' is one native asset's @(qIn, qDelta, qOut)@;
-- every asset must satisfy @qIn + qDelta == qOut@ (for decrement, pass @(qOut, qDelta, qIn)@). Proved to
-- reflect @incrementValueOK@ per asset via @quantityOfᴺ-+ᵛ@ (@incPerAsset→ref@). Catches a selective
-- single-token siphon that leaves the two scalar totals balanced.
checkPerAsset :: [HsAssetIO] -> Bool
checkPerAsset = M.hsCheckPerAsset

-- | Decidable contest checker: version preserved, snapshot strictly increases
-- (@TooOldSnapshot@), exactly one contester appended, posted before the contestation deadline
-- (@validityHiK \<= tfinalK@, the validator's @mustBeWithinContestationPeriod@), and the conditional
-- deadline-UPDATE rule (@tfinalOutK == if contesterLenOut == numPartiesK then tfinalK else tfinalK +
-- cpK@, the validator's @makeContestationDeadline@). Proved to reflect @contestValid@ in
-- @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. The 'HsContestIO' fields are, in order:
-- @versionIn@, @versionOut@, @snapIn@, @snapOut@, @contesterLenIn@, @contesterLenOut@, @tfinalK@,
-- @validityHiK@, @tfinalOutK@, @numPartiesK@, @cpK@ (the last five POSIXTime ms except @numPartiesK@).
-- Crypto\/value is injected.
checkContest :: (HsContestIO -> Bool) -> HsContestIO -> Bool
checkContest = M.hsCheckContest

-- | Decidable fanout checker: all @n+1@ head tokens burned (@burnedCount == n+1@) AND posted after the
-- contestation deadline (@tfinal \< lo@). No @0 \< m@ guard, since the full fanout permits @m == 0@ to
-- finalise an empty head. Accumulator\/value conservation is injected.
checkFanout :: (HsFanout -> Bool) -> HsFanout -> Bool
checkFanout = M.hsCheckFanout

-- | Decidable deposit-recover checker (@deposit.ak@ Recover arm): the recover tx is posted strictly
-- after the recover deadline (@tRecover \< validityLo@, i.e. txValidityMin > t_recover, the
-- validator's @DepositPeriodNotReached@) and this deposit is the only νDeposit input
-- (@depositCountR == 1@, deposit.ak's @single_deposit@: the recovered outputs are positional and
-- shared by every deposit input, so two deposits whose commit list hashes alike would both accept one
-- output set, freeing the second's value); proved to reflect @recoverValid@ in
-- @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. The recovered-outputs hash equality is the
-- injected decision.
checkRecover :: (HsRecoverIO -> Bool) -> HsRecoverIO -> Bool
checkRecover = M.hsCheckRecover

-- | Decidable init token COUNT + PLACEMENT checker (μHead @validateTokensMinting@): the transaction
-- MINTS exactly @n + 1@ tokens of the head policy (@checkNumberOfTokens@) AND those tokens are PLACED in
-- the head output, i.e. the ST is present (@stQty == 1@) and the head output carries exactly @n + 1@
-- head-policy tokens (@headTokenCount == n + 1@). The four 'HsMintIO' fields are @n@, the minted count,
-- the head-output ST quantity, and the head-output head-policy token count. Proved to reflect @initValid@
-- in @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. Seed-spent and datum binding are the injected
-- decision.
checkInit :: (HsMintIO -> Bool) -> HsMintIO -> Bool
checkInit = M.hsCheckInit

-- | Decidable deposit-claim checker (@deposit.ak@ Claim arm): the increment tx collecting the deposit
-- is posted BEFORE the recover deadline (@validityHi \<= tRecover@, i.e. txValidityMax <= t_recover,
-- the validator's @before_deadline@ / @DepositPeriodSurpassed@), the deposit datum's head id equals
-- the spent head's id (@depositCid == headCid@, the head-id half of @expect_increment_redeemer@; the
-- ids are passed as 'HsClaimIO' integer encodings), the redeemer spending the head input has the
-- @Increment@ constructor index 0 (deposit.ak's @is_head_increment@), and that redeemer claims this
-- very deposit (@claimedRefCode == ownRefCode@, the ref half of @expect_increment_redeemer@;
-- otherwise a legitimate increment could carry additional deposit inputs whose value enters the head
-- without any snapshot crediting it; the refs are passed as integer encodings). No injected decision;
-- proved to reflect the joint @claimTxValid@ in @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@.
checkClaim :: HsClaimIO -> Bool
checkClaim = M.hsCheckClaim

-- | Decidable, fully-extractable participant-signature checker (the §5.4-5.7
-- @mustBeSignedByParticipant@, shared by close\/contest\/increment\/decrement): SOME transaction
-- signer holds a participation token. 'HsSignerIO' carries two @[Integer]@ lists, the tx signers'
-- key-hashes (txInfoSignatories) and the head value's PT token-names (a PT's name IS a key-hash),
-- both under the same hash-to-Integer encoding; the check is that they OVERLAP. No injected
-- decision: a non-participant signer (the validator's @SignerIsNotAParticipant@) makes the lists
-- disjoint and the reference reject. Proved to reflect @signedByParticipant@ in
-- @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@ (a postulated extraction-faithfulness boundary).
--
-- Two conditions the real @mustBeSignedByParticipant@ adds are outside this model, so a caller must
-- not read a 'True' here as "the validator accepts". Both make the reference STRICTER or the
-- validator stricter in a way that shows up as a differential disagreement, never as a silent pass:
--
-- * arity: the validator requires EXACTLY one signer (@NoSigners@ \/ @TooManySigners@), while
--   @signedByParticipant@ is an existential over the signer list.
-- * scope: the validator collects participation tokens from ALL transaction inputs, while the spec
--   reads them off the head value alone, which is stricter (a PT parked on an unrelated input
--   satisfies the validator but not this reference).
checkParticipantSigned :: HsSignerIO -> Bool
checkParticipantSigned = M.hsCheckParticipantSigned

-- | Decidable, fully-extractable no-mint\/no-burn checker (the §5.4-5.7 @mustNotMintOrBurn@, shared by
-- close\/contest\/increment\/decrement): the tx mints and burns nothing. The differential supplies the
-- number of non-zero asset entries in @txInfoMint@; the mint is empty exactly when that count is 0. No
-- injected decision: a minting\/burning mutation (the validator's @MintingOrBurningIsForbidden@) makes the
-- count positive and the reference rejects. Proved to reflect @noMint@ in
-- @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@ (a postulated extraction-faithfulness boundary).
checkNoMint :: Integer -> Bool
checkNoMint = M.hsCheckNoMint

-- | Decidable, fully-extractable "referenced output is spent" checker (the increment
-- @claimedDepositIsSpent@ and the μHead @seedInputIsConsumed@): a referenced out-ref is among the tx's
-- spent inputs. The differential supplies the referenced out-ref and the list of the tx's input out-refs,
-- both under one deterministic Integer encoding, and checks membership. No injected decision: spending a
-- different deposit \/ dropping the seed (the validator's @DepositNotSpent@ \/ @SeedNotSpent@) makes the ref
-- absent and the reference rejects. Proved to reflect @depositSpentOK@ in
-- @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@ (a postulated extraction-faithfulness boundary).
checkRefSpent :: Integer -> [Integer] -> Bool
checkRefSpent = M.hsCheckRefSpent

-- | Decidable non-final partial-fanout checker (νHead @checkPartialFanout@, the intermediate
-- FanoutProgress→FanoutProgress batch): at least one output is distributed (@0 \< m@, the
-- @PartialFanoutZeroOutputs@ guard the FULL fanout omits) AND the tx is posted after the contestation
-- deadline (@tfinal \< lo@). The three 'Integer' arguments are @m@, the recorded deadline and the tx lower
-- validity bound (POSIXTime ms). Accumulator\/value conjuncts stay abstract; @mustNotMintOrBurn@ is the
-- shared 'checkNoMint'. Proved to reflect @partialFanoutValid@ in
-- @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@.
checkPartialFanout :: Integer -> Integer -> Integer -> Bool
checkPartialFanout = M.hsCheckPartialFanout

-- | Decidable, fully-extractable value-preservation checker (the close \/ contest
-- @mustPreserveHeadValue@, the exact @==@ on the head value): the ada total AND the non-ada total are
-- unchanged from the head input to the head output. The four 'Integer' arguments are
-- @(adaIn, adaOut, nonAdaIn, nonAdaOut)@; the check is @adaIn == adaOut && nonAdaIn == nonAdaOut@. No
-- injected decision: a value siphon (the validator's @HeadValueIsNotPreserved@) makes a total differ and
-- the reference rejects. Proved to reflect the @valuePreserved@ conjunct of @closeValid@\/@contestValid@ in
-- @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@ via @cong adaOf@\/@cong nonAdaOf@ + @==-sound@ (no new
-- postulate).
checkValuePreserved :: Integer -> Integer -> Integer -> Integer -> Bool
checkValuePreserved = M.hsCheckValuePreserved

-- | Decidable, fully-extractable contest parameter-preservation checker (the scalar half of contest
-- @mustNotChangeParameters@): the produced datum keeps the same head id and contestation period. The four
-- 'Integer' arguments are @(headIdIn, headIdOut, cpIn, cpOut)@ (head ids as their deterministic Integer
-- encoding); the check is @headIdIn == headIdOut && cpIn == cpOut@. No injected decision: re-pointing the
-- head id or changing the period (the validator's @ChangedParameters@) makes a pair differ and the
-- reference rejects. Proved to reflect the contest transition's parameter preservation in
-- @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@ via @cong cidToNat@ + @==-sound@ (no new postulate;
-- the @parties@-list half stays a documented boundary, as the spec abstracts parties into a count).
checkContestParams :: Integer -> Integer -> Integer -> Integer -> Bool
checkContestParams = M.hsCheckContestParams

-- | Decidable, fully-extractable init datum head-id binding (the decidable half of the μHead
-- @checkDatum@): the head output datum declares its own minting policy as its head id
-- (@datumHeadId == currency@). The two 'Integer' arguments are the head-id and currency Integer
-- encodings. No injected decision: a datum naming a different head id (the validator's @WrongDatum@) makes
-- the pair differ and the reference rejects. Proved to reflect the init datum binding in
-- @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@ via the existing @cidToNat@ encoding (no new
-- postulate). The @seed == seedInput@ half and the @cid = hash(seed)@ binding stay documented boundaries.
checkInitHeadId :: Integer -> Integer -> Bool
checkInitHeadId = M.hsCheckInitHeadId

-- | Decidable, fully-extractable μHead Burn-arm checker (@validateTokensBurning@): every head-policy
-- entry of the mint field is negative, i.e. no positive entry exists and at least one negative one does
-- (the real policy rejects a mint field without head-policy entries). The two 'HsBurnIO' fields are
-- the counts of the positive and negative head-policy mint entries. No injected decision. Proved to
-- reflect @burnValid@ in @spec\/src\/Hydra\/Protocol\/ReferenceBridge.agda@. Zero-quantity mint
-- entries are outside the domain (not representable in canonical ledger values). WHICH burns are
-- legitimate is νHead's concern (the fan-out family's burn count); μHead only guarantees burn-only.
checkBurn :: HsBurnIO -> Bool
checkBurn = M.hsCheckBurn
