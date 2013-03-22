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
    cwd <- getCurrentDirectory
    let config = A.Config (cwd ++ "/assets/")
    quickHttpServe $ site config

site :: A.Config -> Snap ()
site config = A.snapAssetHandler config assets
