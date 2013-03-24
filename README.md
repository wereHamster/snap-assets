Snap Asset Pipeline
-------------------

This package provides tools and helpers to integrate a Ruby on Rails-like
asset pipeline into Snap. The asset pipeline serves two purposes:

 - Allows you to build/compile/preprocess your assets. A common example would
   be to concatenate multiple JavaScript files into a single file. Or
   compiling your CSS files using SASS/Stylus/Compass etc.

 - Fingerprints files so you can set a Expires header far in the future, but
   still have a way to force the client to refresh it.


Getting Started
---------------

First, you have to define the assets. Each asset has a name (that is what will
be used in the URL to request it) and a builder. The builder defines how to
build the asset. The builder runs in the IO monad, so you can read files, fork
external processes etc. It's also perfectly valid to have a piece of pure
Haskell code that builds the asset (for example using Clay to generate CSS).

The snap asset pipeline comes with one builder: `concatBuilder`. The concat
builder will simply concatenate all the files and serve them to the client as
a single blob. Let's use it to define our application JavaScript file:

    assets :: [ Asset ]
    assets =
      [ Asset "application.js" $ concatBuilder
        [ "javascript/jquery.js"
        , "javascript/app.js"
        ]
      ]

By default, the builder reads the assets from a directory `assets`. So let's
create the two files, `jquery.js` and `app.js`:

    $ mkdir -p assets/javascript
    $ echo "// jquery.js" > assets/javascript/jquery.js
    $ echo "// app.js"    > assets/javascript/app.js

Sweet, we have our asset. The pipeline uses a `Config` structure to store
various configuration options used by the pipeline. The defaults are
reasonable, we just have to supply our asset list:

    loadConfig :: IO Config
    loadConfig = do
        config <- defaultConfig
        return $ config { assetDefinitions = assets }

Now, in development mode, you can use the `snapAssetHandler` to serve the
assets to clients. The handler simply runs the builder and streams the result
to the client. It uses minimal caching, just to be useful in development mode.

    main :: IO ()
    main = do
        config <- loadConfig
        quickHttpServe $ snapAssetHandler config

If you start the web server, you can request the "application.js" file. What
you will see is the two javascript files concatenated:

    $ curl http://localhost:8000/assets/application.js
    // jquery.js
    // app.js
    $

Now you probably have a template and want to insert the url to this
application.js into your html. Use `assetUrl` for that:

    site :: Config -> Snap ()
    site config = do
        appJsUrl <- assetUrl config "application.js"
        -- appJsUrl == "//localhost:8000/assets/application.js"

        ...

Fingerprinting
--------------

Fingerprinting is the process of adding a unique fingerprint to each asset.
This fingerprint is generated from the asset contents. If the contents change,
so does the fingerprint. This allows you to set the Expires header far in the
future, without having to worry about stale files on the client. You do this
in production mode so that clients can cache the files, that speeds up your
website.

You usually serve the assets from a static file server or a CDN. So you first
have to compile them. The snap asset pipeline has a function which will do
that and output the assets into an output directory. You can then upload the
contents of the output directory to your file server.

The function will return a manifest. That is a mapping from asset name to its
fingerprint. This is needed in production mode so that `assetUrl` can generate
the correct urls. You have to store the manifest somewhere and load it in
production mode.

    prepareAssetsForDeployment :: Config -> IO ()
    prepareAssetsForDeployment config = do
        manifest <- compileAssets config "output-directory"
        dumpManifest manifest "manifest.mf"

The contents of `output-directory` will look something like this:

    $ find output-directory
    output-directory/
    output-directory/assets
    output-directory/assets/application-a8aaf357acfc18f55dc6d93ae875e38babdd0d08.js
    $

In production, you use a different config. Instead of supplying asset
definitions, you supply the manifest.

    productionAssetConfig :: IO Config
    productionAssetConfig = do
        config   <- defaultConfig
        manifest <- loadManifest "manifest.mf"
        return $ config { manifest = manifest }

With this production config, `assetUrl config "application.js"` will return
a different url. Something like this:

    "//localhost:8000/assets/application-a8aaf357acfc18f55dc6d93ae875e38babdd0d08.js"



Asset Path Helpers
------------------

Notice that in production mode, the assets have different url. This url is
only known once you compile the asset. So when you reference an image from
a CSS file, you have to insert special placeholders which will be resolved
upon compilation.

    body {
        background-image: url(${images/background.png});
    }

will turn into something like this:

    body {
        background-image: url(/images/background-c10a4572b80930ff9c13c4f09269bfe809c06a48.png);
    }

Caveat: It actually doesn't work that way. The templating langauge that is
used now doesn't support that. And using `$` as the escape symbol is very bad,
especially for JavaScript sources where the dollar sign often appears, so
you'd have to escape it all the time. We'll have to switch to a different
templating system to fix that. Suggestions which alternatives are available
are very welcome.
