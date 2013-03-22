module Snap.Assets where


import Data.Time.Clock
import Data.ByteString
import Snap.Core
import qualified Data.Map as M


data Config = Config
    { configRoot :: String
      -- ^ The root path where the assets are kept. Paths you use in the
      --   builders are relative to this root. Most projects will have an
      --   'assets/' directory where all the javascript, css, images are kept.
      --   The asset pipeline reads from that directory and serves those files
      --   to clients.
    }


-- | Describes an asset that is served to the clients. Each asset has a name
--   (that is the string that is used in the incoming requests' URL) and
--   a builder which specifies how to build the asset. The contents are
--   basically a ByteString which is sent in the response back to the client.
data Asset = Asset
    { assetName :: String
    , assetBuilder :: Builder
    }


-- | Builds (compiles, generates, ..) the contents of an asset. Runs in the IO
--   monad so you can do file IO etc. The builder should also have access to
--   the Config so it knows where the root is.
type Builder =
       Config        -- ^ Config so the builder knows where the root is
    -> Maybe UTCTime -- ^ The contents of the If-Modified-Since header,
                     --   so the builder can return NotModified if
                     --   appropriate.
    -> IO BuilderResult

data BuilderResult
    = NotModified
      -- ^ The contents have not been modified since the last request. This is
      --   especially important in development mode where you don't want to be
      --   needlessly rebuilding the assets.

    | Contents ByteString


-- | Concatenates all files into a single blob.
concatBuilder :: [ FilePath ] -> Builder
concatBuilder files _ _ = undefined -- read all files, concatenate them


-- | This builder builds the file using browserify. That tool is nice because
--   it automatically traverses all dependencies by scanning JavaScript files
--   and looking for calls to require(...).
browserifyBuilder :: FilePath -> Builder
browserifyBuilder entryPoint _ _ = undefined
    -- spawn the browserify tool to build the javascript file.
    -- System.Process.createProcess "browserify <path>", capture the stdout of
    -- that command and that's what we want to return.



-- | All assets that we can compile. Instead of automatically serving all
--   files in the asset root directory, we have a 'whitelist'. So that
--   a client can't request arbitrary files.
assets :: [ Asset ]
assets =
    [ Asset "frameworks.js" $ concatBuilder
        [ "frameworks/jquery-1.7.1.js"
        , "frameworks/backbone.js"
        , "frameworks/jquery.plugin-foo.js"
        ]

    , Asset "client.js" $ browserifyBuilder "client/entry.js"
    ]


-- In development mode, there is a snap handler which will serve the assets as
-- we have configured them, simply streaming the output from the builder back
-- to the client.

snapAssetHandler :: Config -> [ Asset ] -> Snap ()
snapAssetHandler config assets = do
    -- 1. Extract the path from the request.
    -- 2. See if it starts with /assets/.
    -- 3. Find the asset in the asset list.
    -- 4. Run the builder and return the contents to the client.

    return ()


-- Now, in production mode we need to fingerprint the files. Eg. turn
-- "frameworks.js" into "frameworks-908e25f4bf641868d8683022a5b62f54.js".
-- The large hash is the SHA1 of the file contents, so we can set a Expires
-- header far in the future, but still force the client to refetch the file if
-- the contents change (because then the fingerprint changes).
--
-- If you have templates which reference the "frameworks.js" asset, you need
-- to know the files fingerprint before you can generate the html. So in
-- production mode you precompile the assets and generate a manifest, with
-- mapping from asset name -> fingerprint. The template engine will then use
-- this manifest file to insert the correct urls into the templates.

type Manifest = M.Map String String
precompileAssets :: Config -> [ Asset ] -> String -> IO Manifest
precompileAssets config assets outputDirectory = do
    -- For each asset, run the builder to generate the contents, then take the
    -- SHA1 of that and add to the manifest. Write the generated files into
    -- the output directory, using the correct (fingerprinted) name.
    -- The idea is then to upload the contents of the output directory to your
    -- CDN, so it can be served by fast/dedicated servers.

    return M.empty


-- I have that much written in JavaScript and it's in use in encounter. But
-- there is an additional complexity: What if you reference assets from other
-- assets? Think images referenced from css files. So the builders need to
-- preprocess all files to see if that is happening, so they can insert the
-- proper urls.
--
-- In the Rails asset pipeline you'd write something like this in your CSS
-- file (ERB syntax):
--
--     .foo {
--         background-url: url(<%= asset_path "images/something.png" %>);
--     }

-- The case of referencing an image from a CSS file is usually no problem. But
-- if two files reference each other, you'll get into trouble. So ideally
-- you'd detect that instead of blowing up your stack.


assetFingerprint :: Config -> [ Asset ] -> Asset -> IO String
assetFingerprint config assets asset = do
    -- Run the builder to get the contents, and create the fingerprint (SHA1).
    -- Because the fingerprint can depend on other assets, this function needs
    -- to know the config and the list of all assets.

    return undefined


fingerprintedAssetName :: String -> String -> String
fingerprintedAssetName name fingerprint =
    -- Split the name into basename + extension ["frameworks", ".js"], insert
    -- ["-", fingerprint] in the middle, then join the parts again.

    return undefined
