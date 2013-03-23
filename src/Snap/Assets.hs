module Snap.Assets
  (
    -- * Configuration
    Config
  , Manifest
  , defaultConfig

    -- * The Asset type
  , Asset(..)

    -- * Builers
  , concatBuilder

    -- * Snap handlers
  , snapAssetHandler

    -- * Template helpers
  , assetUrl

    -- * Tooling support
  , compileAssets

  ) where


import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as C
import           Data.Digest.Pure.SHA
import qualified Data.Map              as M
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

import           Snap.Core

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class

import           System.Directory
import           System.FilePath.Posix


data Config = Config
    { sourceDirectory :: String
      -- ^ The root path where the assets are kept. Paths you use in the
      --   builders are relative to this root. Most projects will have an
      --   'assets/' directory where all the javascript, css, images are kept.
      --   The asset pipeline reads from that directory and serves those files
      --   to clients.

    , assetDefinitions :: [ Asset ]
      -- ^ The list of all assets that are known and served to clients. It is
      --   used by the snap handler and compiler to generate the files. If you
      --   have the snap handler disabled (eg. when running in production
      --   mode), it's safe to leave this list empty.

    , assetHost :: Snap String
      -- ^ The asset host used to serve the assets. Used by 'assetUrl' and
      --   'assetPath' to generate the urls. Also see 'pathPrefix'. You
      --   generally have two choices:
      --
      --     * Extract the host from the request (default, see 'fromRequest').
      --
      --     * Use a single fixed host (eg. if you want to use a CDN).

    , pathPrefix :: String
      -- ^ This prefix is added to paths generated by 'assetPath' and
      --   'assetUrl'. Default is @/assets@. If you have a dedicated asset
      --   host, you can leave this an empty string. The two most often used
      --   combinations of 'assetHost' and 'pathPrefix' are:
      --
      --
      --   * Using a dedicated asset host (or a CDN).
      --
      --      > config = { assetHost = return "assets.domain.tld", pathPrefix = "" }
      --      >
      --      >   assetUrl   ->  "http://assets.domain.tld/file.js"
      --      >   assetPath  ->  "http://assets.domain.tld/file.js"
      --
      --
      --   * Serving assets from a subdirectory of the same host as your app.
      --
      --      > config = { assetHost = fromRequest, pathPrefix = "/assets" }
      --      >
      --      >   assetUrl   ->  "http://domain.tld/assets/file.js"
      --      >   assetPath  ->  "/assets/file.js"

    , manifest :: Maybe Manifest
      -- ^ If set, asset urls and paths will be fingerprinted using the hashes
      --   stored in the manifest. This is something you'd want to enable when
      --   your application is running in production mode. You can store the
      --   manifest on disk (as YAML, JSON, plain text etc) and load when your
      --   application is starting, or compile it into your app.
    }

defaultConfig :: IO Config
defaultConfig = do
    cwd <- getCurrentDirectory
    return $ Config
        { sourceDirectory  = cwd ++ "/assets/"
        , assetDefinitions = []
        , assetHost        = fromRequest
        , pathPrefix       = "/assets"
        , manifest         = Nothing
        }

fromRequest :: Snap String
fromRequest = C.unpack <$> rqServerName <$> getRequest


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

  deriving Show


-- | Concatenate all files into a single blob.
concatBuilder :: [ FilePath ] -> Builder
concatBuilder files config ifModifiedSince = do
    case ifModifiedSince of
        Nothing -> build
        Just t  -> do
            mtime <- foldM newestModificationTime t paths
            if mtime == t
                then return NotModified
                else build

  where

    root  = sourceDirectory config
    paths = map (root++) files
    build = Contents <$> L.concat <$> mapM L.readFile paths

    newestModificationTime :: UTCTime -> FilePath -> IO UTCTime
    newestModificationTime acc path =
        max acc <$> getModificationTime path


-- | This builder builds the file using browserify. That tool is nice because
--   it automatically traverses all dependencies by scanning JavaScript files
--   and looking for calls to require(...).
browserifyBuilder :: FilePath -> Builder
browserifyBuilder entryPoint _ _ = undefined
    -- spawn the browserify tool to build the javascript file.
    -- System.Process.createProcess "browserify <path>", capture the stdout of
    -- that command and that's what we want to return.



-- In development mode, there is a snap handler which will serve the assets as
-- we have configured them, simply streaming the output from the builder back
-- to the client.

snapAssetHandler :: Config -> Snap ()
snapAssetHandler config = route assetRoutes
  where

    prefix      = pathPrefix config
    assetRoutes = map snapHandler (assetDefinitions config)

    snapHandler :: Asset -> ( S.ByteString, Snap () )
    snapHandler asset = ( C.pack (prefix ++ "/" ++ assetName asset), assetHandler asset )

    assetHandler :: Asset -> Snap ()
    assetHandler asset = do
        req <- getRequest
        let mbH = getHeader "If-Modified-Since" req
        ifModifiedSince <- case mbH of
            Nothing  -> return Nothing
            (Just s) -> liftIO $ liftM Just $ parseHttpTime s

        result <- liftIO $ (assetBuilder asset) config (toUTCTime ifModifiedSince)
        case result of
            NotModified -> notModified
            Contents x  -> writeLBS x

    notModified = modifyResponse $ setResponseStatus 304 "Not Modified"
    toUTCTime   = liftM $ posixSecondsToUTCTime . realToFrac


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

-- | For each asset, run the builder to generate the contents, then take the
--   SHA1 of that and add to the manifest. Write the generated files into
--   the output directory, using the correct (fingerprinted) name.
--   The idea is then to upload the contents of the output directory to your
--   CDN, so it can be served by fast/dedicated servers.
compileAssets :: Config -> String -> IO Manifest
compileAssets config outputDirectory = do
    createDirectoryIfMissing True outputDirectory
    foldM updateManifest M.empty (assetDefinitions config)

  where

    prefix = pathPrefix config

    updateManifest :: Manifest -> Asset -> IO Manifest
    updateManifest manifest asset = do
        fingerprint <- assetFingerprint config asset
        let name = fingerprintedAssetName (assetName asset) fingerprint

        result <- (assetBuilder asset) config Nothing
        case result of
            NotModified -> error "noway"
            Contents contents -> do
                let dir = (outputDirectory ++ "/" ++ prefix)

                parsedContents <- resolveReferences config contents
                createDirectoryIfMissing True dir
                L.writeFile (dir ++ "/" ++ name) parsedContents

                return $ M.insert (assetName asset) fingerprint manifest





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


-- | Run the builder to get the contents, and create the fingerprint (SHA1).
--   Because the fingerprint can depend on other assets, this function needs
--   to know the config and the list of all assets.
assetFingerprint :: Config -> Asset -> IO String
assetFingerprint config asset = do
    result <- (assetBuilder asset) config Nothing
    case result of
        NotModified -> error "noway"
        Contents contents -> do
            parsedContents <- resolveReferences config contents
            return $ showDigest $ sha1 parsedContents



-- | Split the name into basename + extension ["frameworks", ".js"], insert
--   ["-", fingerprint] in the middle, then join the parts again.
fingerprintedAssetName :: String -> String -> String
fingerprintedAssetName name fingerprint =
    let split = splitExtension name
    in (fst split) ++ "-" ++ fingerprint ++ (snd split)


-- | This function takes the contents of an asset and replaces all references
--   to other assets with the correct urls.
--
--   The individual builders may want to decide whether to use this function
--   or not. For example, it makes sense to use it on JavaScript or CSS files,
--   but not so much on images.
resolveReferences :: Config -> ByteString -> IO ByteString
resolveReferences config contents = do

    -- Scan the contents for magic instructions such as <%= asset_path ... %>
    -- and replace those references with the correct urls. This may need to be
    -- done recursively.
    --
    -- That requires keeping some kind of state, such as the list of files
    -- you've already processed etc. But this is getting way over my head
    -- and/or haskell skills.

    -- For now, this function is a noop.
    return contents


-- Lastly, it is desired to compress/minify the files in production mode. This
-- can be done either implicitly (detect filename extension and run the
-- correct compressor depending on the file type) or explicitly by
-- enabling/disabling them in the configuration.

minifyJavascript :: Config -> Maybe UTCTime -> BuilderResult -> IO BuilderResult
minifyJavascript config lastModified result =
    case result of
        NotModified       -> return NotModified
        Contents contents -> Contents <$> uglify contents

  where

    uglify :: ByteString -> IO ByteString
    uglify x = undefined
    -- Spawn the 'uglifyjs' commandline utility, feed it the pretty javascript
    -- and capture its output.


-- | Generate the asset url for the asset with the given name. If we have
--   a manifest we fingerprint the name with the hash from the manifest.
assetUrl :: Config -> String -> Snap String
assetUrl config name = do
    host <- assetHost config
    case (manifest config) of
        Nothing       -> buildUrl host name
        Just manifest ->
            case M.lookup name manifest of
                Nothing -> error $ "Asset " ++ name ++ " not in the manifest"
                Just fingerprint -> buildUrl host (fingerprintedAssetName name fingerprint)

  where

    prefix             = pathPrefix config
    buildUrl host path = return $ "//" ++ host ++ prefix ++ "/" ++ path
