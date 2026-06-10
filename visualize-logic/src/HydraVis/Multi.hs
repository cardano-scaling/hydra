{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

-- | Multi-party simulation. A 'MultiModel' wraps N single-party 'Model's
-- keyed by 'Party'; the user picks one tab at a time and authors inputs
-- against it. Whenever the focused party's outcome contains a
-- 'NetworkEffect' broadcast, we synthesise a 'NetworkInput' on every other
-- party so the message propagates through the simulated head.
module HydraVis.Multi (
  MultiModel (..),
  MultiAction (..),
  PartySpec (..),
  mkMultiApp,
  mkMultiModel,
) where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Hydra.Chain.ChainState (IsChainState)
import Hydra.HeadLogic qualified as HL
import Hydra.HeadLogic.Input (Input (..))
import Hydra.HeadLogic.Outcome (Effect (NetworkEffect), Outcome (Continue), StateChanged)
import Hydra.Ledger (Ledger)
import Hydra.Network.Message (Message, NetworkEvent (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (NodeState, initialChainTime)
import Hydra.Tx.Party (Party)
import HydraVis.History (HistoryStep (..))
import HydraVis.UI (Action (..), AuthoringCtx (..), Model (..))
import HydraVis.UI qualified as UI
import Miso (App (..), View, defaultEvents, noEff)
import Miso.Effect qualified as ME
import Miso.Html (button_, div_, h1_, onClick, text)
import Miso.Html.Property (styleInline_)
import Miso.String (MisoString, ms)
import Miso.String qualified as MS
import Miso.Types (LogLevel (..))

-- | What the caller passes for each simulated party.
data PartySpec tx = PartySpec
  { specEnvironment :: Environment
  , specLedger :: Ledger tx
  , specPresets :: [(MisoString, Input tx)]
  , specInitialState :: NodeState tx
  }

data MultiModel tx = MultiModel
  { partyModels :: Map Party (Model tx)
  , focus :: Party
  , partyOrder :: [Party]
  , networkTtl :: Word
  -- ^ Round-robin sender we attribute synthesised 'NetworkInput's to so
  -- 'HeadLogic.update' has a valid party tag.
  }
  deriving stock (Generic)

deriving stock instance (IsChainState tx, Eq (NodeState tx)) => Eq (MultiModel tx)

data MultiAction tx
  = MNoOp
  | Focus Party
  | ForParty Party (Action tx)
  deriving stock (Generic)

mkMultiModel :: [PartySpec tx] -> MultiModel tx
mkMultiModel [] = error "HydraVis.Multi.mkMultiModel: empty party list"
mkMultiModel specs =
  let entry s =
        ( party (specEnvironment s)
        , UI.mkModel
            ( Just
                AuthoringCtx
                  { ctxEnvironment = specEnvironment s
                  , ctxLedger = specLedger s
                  , ctxPresets = specPresets s
                  }
            )
            Nothing
            False
            (specInitialState s)
            0
            []
        )
      entries = map entry specs
   in MultiModel
        { partyModels = Map.fromList entries
        , focus = fst (case entries of (e : _) -> e; [] -> error "impossible")
        , partyOrder = map fst entries
        , networkTtl = 5
        }

mkMultiApp ::
  ( IsChainState tx
  , FromJSON (Input tx)
  , ToJSON (Input tx)
  , ToJSON (NodeState tx)
  , ToJSON (StateChanged tx)
  ) =>
  MultiModel tx ->
  App (MultiModel tx) (MultiAction tx)
mkMultiApp initialModel =
  App
    { initialAction = MNoOp
    , model = initialModel
    , update = updateMulti
    , view = viewMulti
    , subs = []
    , events = defaultEvents
    , mountPoint = Nothing
    , logLevel = Off
    }

updateMulti ::
  forall tx.
  (IsChainState tx, FromJSON (Input tx)) =>
  MultiAction tx ->
  MultiModel tx ->
  ME.Effect (MultiAction tx) (MultiModel tx)
updateMulti act m = case act of
  MNoOp -> noEff m
  Focus p -> noEff m{focus = p}
  ForParty p a -> noEff (routeAction p a m)

routeAction ::
  forall tx.
  (IsChainState tx, FromJSON (Input tx)) =>
  Party ->
  Action tx ->
  MultiModel tx ->
  MultiModel tx
routeAction p a m = case Map.lookup p (partyModels m) of
  Nothing -> m
  Just senderModel ->
    let broadcasts = case a of
          ApplyDraft -> outcomeBroadcasts senderModel
          _ -> []
        senderModel' = UI.applyAction a senderModel
        deliveredEverywhere = applyBroadcastsToOthers p broadcasts (networkTtl m) (partyModels m)
        finalModels = Map.insert p senderModel' deliveredEverywhere
     in m{partyModels = finalModels}

-- | Inspect the party model's draft and, if it parses as an 'Input', run
-- 'HeadLogic.update' to see what would be broadcast.
outcomeBroadcasts ::
  forall tx.
  IsChainState tx =>
  Model tx ->
  [Message tx]
outcomeBroadcasts pm = case ctx pm of
  Nothing -> []
  Just AuthoringCtx{ctxEnvironment, ctxLedger} ->
    case Aeson.eitherDecodeStrict' (TE.encodeUtf8 draftText) of
      Left _ -> []
      Right input ->
        let priorState = case nonEmpty (V.toList (history pm)) of
              Nothing -> initial pm
              Just ne -> stateAfter (last ne)
            outcome :: Outcome tx
            outcome = HL.update ctxEnvironment ctxLedger initialChainTime priorState input
         in case outcome of
              Continue{effects} -> [msg | NetworkEffect{message = msg} <- effects]
              _ -> []
 where
  draftText :: Text
  draftText = MS.fromMisoString (inputDraft pm)

applyBroadcastsToOthers ::
  forall tx.
  IsChainState tx =>
  Party ->
  [Message tx] ->
  Word ->
  Map Party (Model tx) ->
  Map Party (Model tx)
applyBroadcastsToOthers sender msgs ttl =
  Map.mapWithKey $ \q qm ->
    if q == sender
      then qm
      else foldl' (deliverOne sender ttl) qm msgs

deliverOne ::
  forall tx.
  IsChainState tx =>
  Party ->
  Word ->
  Model tx ->
  Message tx ->
  Model tx
deliverOne sender ttl m msg = case ctx m of
  Nothing -> m
  Just AuthoringCtx{ctxEnvironment, ctxLedger} ->
    UI.applyInputPure
      ctxEnvironment
      ctxLedger
      (NetworkInput{ttl = fromIntegral ttl, networkEvent = ReceivedMessage{sender, msg}})
      m

viewMulti ::
  (IsChainState tx, ToJSON (Input tx), ToJSON (NodeState tx), ToJSON (StateChanged tx)) =>
  MultiModel tx ->
  View (MultiAction tx)
viewMulti m =
  div_
    [styleInline_ "font-family: system-ui, sans-serif; max-width: 1100px; margin: 1rem auto; padding: 0 1rem;"]
    [ h1_ [] [text "hydra-vis (multi-party)"]
    , partyTabs m
    , case Map.lookup (focus m) (partyModels m) of
        Nothing -> div_ [] [text "no party focused"]
        Just pm -> fmap (ForParty (focus m)) (UI.viewModel pm)
    ]

partyTabs :: MultiModel tx -> View (MultiAction tx)
partyTabs m =
  div_
    [styleInline_ "display: flex; gap: 0.5rem; margin-bottom: 1rem;"]
    [ button_
      [ onClick (Focus p)
      , styleInline_ $
          "padding: 0.5rem 1rem; cursor: pointer; border-radius: 4px; "
            <> if p == focus m
              then "background: #cce5ff; border: 1px solid #6ab;"
              else "background: #fff; border: 1px solid #ccc;"
      ]
      [text (partyLabel p)]
    | p <- partyOrder m
    ]

-- | Show only the trailing chars of the verification-key so the three
-- canonical fixtures (alice/bob/carol) are visually distinguishable.
partyLabel :: Party -> MisoString
partyLabel p =
  let raw :: Text
      raw = show p
      hex = T.drop (T.length "Party {vkey = \"") raw
      short = T.take 8 hex
   in ms ("party:" <> short)
