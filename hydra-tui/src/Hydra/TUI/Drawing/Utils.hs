module Hydra.TUI.Drawing.Utils where

import Brick (Widget, emptyWidget, txt)
import Data.Text qualified as Text
import Hydra.Cardano.Api (SerialiseAsRawBytes, TxId, serialiseToRawBytesHexText)
import Hydra.Prelude
import Hydra.Tx (HeadId)

-- | Render a 'HeadId' as its hex-encoded bytes (matches the Main tab display).
prettyHeadId :: HeadId -> Text
prettyHeadId = serialiseToRawBytesHexText

-- | Render a 'TxId' as its hex-encoded bytes (no quotes, no wrapper type).
prettyTxId :: TxId -> Text
prettyTxId = serialiseToRawBytesHexText

drawHex :: SerialiseAsRawBytes a => a -> Widget n
drawHex = txt . (" - " <>) . serialiseToRawBytesHexText

drawShow :: forall a n. Show a => a -> Widget n
drawShow = txt . show

maybeWidget :: (a -> Widget n) -> Maybe a -> Widget n
maybeWidget = maybe emptyWidget

ellipsize :: Int -> Text -> Text
ellipsize n t = Text.take (n - 2) t <> ".."
