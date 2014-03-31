import Distribution.Simple (defaultMainWithHooks)
import Distribution.Simple.UUAGC (uuagcLibUserHook)
import UU.UUAGC (uuagc)

{-
main :: IO ()
main = defaultMainWithHooks $
         uuagcLibUserHook uuagc
-}

{-
-}
import Distribution.PackageDescription
import Distribution.Simple.UserHooks
import Distribution.Package
import Data.Version

main :: IO ()
main = defaultMainWithHooks $
         addHook $
         uuagcLibUserHook uuagc
  where addHook hooks = hooks {
            postConf = postConf_InsertVersion hooks
          }
        -- postConf_InsertVersion :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
        postConf_InsertVersion hooks args cfg pkgDescr bi = do
          postConf hooks args cfg pkgDescr bi
          writeFile "src/UHC/Shuffle/Version.hs" $
            "module UHC.Shuffle.Version where\n" ++
            "version = \"" ++ showVersion (pkgVersion $ package pkgDescr) ++ "\"\n"
