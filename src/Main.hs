module Main where

import qualified Snap.Assets as A
import Snap.Core
import Snap.Http.Server
import System.Directory
import Control.Applicative
import Control.Monad.IO.Class


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
    let config1 = config0 { A.assetDefinitions = assets }

    manifest <- A.compileAssets config1 "output"
    putStrLn $ show manifest

    let config2 = config1 { A.manifest = Just manifest }

    quickHttpServe $ site $ config2

site :: A.Config -> Snap ()
site config = do
    liftIO $ putStrLn $ A.assetPath config "frameworks.js"

    url <- A.assetUrl config "frameworks.js"
    liftIO $ putStrLn url
    A.snapAssetHandler config
