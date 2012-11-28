-------------------------------------------------------------------------
-- Common stuff based on CDoc
-------------------------------------------------------------------------

module UHC.Shuffle.CDocCommon
  ( module UHC.Shuffle.Common
  , module UHC.Shuffle.CDoc
  , NmChInfo(..), NmChMp
  )
  where

import UHC.Shuffle.Common
import UHC.Shuffle.CDoc
import qualified Data.Map as Map

-------------------------------------------------------------------------
-- Named chunks
-------------------------------------------------------------------------

data NmChInfo
  = NmChInfo
      { nciNm       :: CRef
      , nciChDest   :: ChDest
      , nciMbCDoc   :: Maybe CDoc
      , nciMkCDoc   :: MkCDoc
      }

type NmChMp = Map.Map CRef NmChInfo

