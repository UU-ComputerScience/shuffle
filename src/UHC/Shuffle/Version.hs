module UHC.Shuffle.Version (version) where

import qualified Paths_shuffle (version)
import Data.Version (showVersion)

version :: String
version = showVersion Paths_shuffle.version
