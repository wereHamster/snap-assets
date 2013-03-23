module Main where

import qualified Snap.Assets as A
import Snap.Core
import Snap.Http.Server
import System.Directory
import Control.Applicative


assets :: [ A.Asset ]
assets =
    [ A.Asset "frameworks.js" $ A.concatBuilder
        [ "frameworks/jquery-1.7.1.js"
        , "frameworks/backbone.js"
        , "frameworks/jquery.plugin-foo.js"
        ]
    ]


main :: IO ()
main = do
    config0 <- A.defaultConfig
    let config = config0 { A.assetDefinitions = assets }
    fingerprint <- A.assetFingerprint config (head assets)
    putStrLn fingerprint

    manifest <- A.precompileAssets config "output"
    putStrLn $ show manifest