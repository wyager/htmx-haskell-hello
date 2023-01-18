module Main (main) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (async,  waitAny)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as TLS
import qualified Options.Applicative as OA
import Servant
  ( 
   --  Accept,
    Get,
   --  MimeRender,
  --  OctetStream,
    Proxy (Proxy),
  --  QueryParam,
   -- QueryParams,
    Raw,
  --  RemoteHost,
    Server,
  --  contentType,
  --  mimeRender,
    serve,
    serveDirectoryWith,
    (:<|>) ((:<|>)),
    (:>),
  )
import Servant.HTML.Blaze (HTML)
import System.FilePath.Posix (addTrailingPathSeparator)
import System.IO
  ( IOMode (AppendMode),
    hPrint,
    openFile,
  )
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
-- import qualified URI.ByteString as URIB
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)

type Site =
    "a" :>  Get '[HTML] H.Html
    :<|> "b" :>  Get '[HTML] H.Html
    :<|> Get '[HTML] H.Html
    :<|> Raw

site :: Proxy Site
site = Proxy

serveDirectory :: FilePath -> Server Raw
serveDirectory = serveDirectoryWith . defaultWebAppSettings . addTrailingPathSeparator

style :: H.Html
style = H.style $ do
    "body {background-color: linen;}"
    "p {color: maroon;}"
    ".buttonDiv {color: blue;}"
    "@font-face {font-family: 'Berkeley Mono'; src: url('/BerkeleyMono-Regular.woff2') format('woff2');}"
    "span.mono {font-family: 'Berkeley Mono';}"

a :: H.Html 
a = H.div ! hxGet "/b" ! outer ! A.class_ "buttonDiv" $ "Click me :)"

b :: H.Html
b = H.div ! hxGet "/a" ! outer ! A.class_ "buttonDiv" $ "Click me :o"


hxGet :: B.AttributeValue -> B.Attribute
hxGet = B.customAttribute "hx-get"

hxSwap :: B.AttributeValue -> B.Attribute
hxSwap = B.customAttribute "hx-swap"

outer :: B.Attribute
outer = hxSwap "outerHTML"

index :: H.Html
index = H.body $ do
    H.head $ do
        style
        H.script ! A.src "/htmx.min.js" $ ""
    H.body $ do
        H.p $  do
            "Welcome to "
            H.span ! A.class_ "mono" $ "<The index>"
        a
    

makeServer :: FilePath -> IO (Server Site)
makeServer contentPath = do
  let files = serveDirectory contentPath
      server =
           return a :<|> return b :<|> return index :<|> files
  return server

data TLSMode = NoTLS | TLS {tlsCertPath :: FilePath, tlsKeyPath :: FilePath, tlsPort :: Warp.Port}

tlsModeArgs :: OA.Parser TLSMode
tlsModeArgs = noTLS <|> (TLS <$> tlsCertPath <*> tlsKeyPath <*> tlsPort)
  where
    noTLS = OA.flag' NoTLS (OA.long "no-tls")
    tlsPort = OA.option OA.auto (OA.long "tls-port" <> OA.value 443 <> OA.showDefault)
    tlsCertPath = OA.option OA.str (OA.long "tls-cert-path" <> OA.metavar "TLS_CERT_PATH")
    tlsKeyPath = OA.option OA.str (OA.long "tls-key-path" <> OA.metavar "TLS_KEY_PATH")

data Command = Serve {logPath :: FilePath, contentPath :: FilePath, httpPort :: Warp.Port, tlsMode :: TLSMode}

command :: OA.Parser Command
command = serveCmd
  where
    serverSwitch = OA.flag' () (OA.long "server" <> OA.short 's')
    logPath = OA.option OA.str (OA.long "log-path" <> OA.short 'l' <> OA.metavar "LOG_PATH")
    contentPath = OA.option OA.str (OA.long "content-path" <> OA.short 'c' <> OA.metavar "CONTENT_PATH")
    httpPort = OA.option OA.auto (OA.long "http-port" <> OA.value 80 <> OA.showDefault)
    serveCmd = serverSwitch *> (Serve <$> logPath <*> contentPath <*> httpPort <*> tlsModeArgs)

main :: IO ()
main = do
  OA.execParser (OA.info (OA.helper <*> command) OA.fullDesc) >>= \case
    Serve {..} -> do
      logHandle <- openFile logPath AppendMode
      server <- makeServer contentPath
      let application = \req respond -> do
            hPrint logHandle req
            serve site server req respond
          http = Warp.runSettings (Warp.setPort httpPort Warp.defaultSettings) application
          https tlsSettings tlsPort = TLS.runTLS tlsSettings (Warp.setPort tlsPort Warp.defaultSettings) application
      case tlsMode of
        NoTLS -> http
        TLS {..} -> do
          let tlsSettings = TLS.tlsSettings tlsCertPath tlsKeyPath
          http' <- async http
          https' <- async (https tlsSettings tlsPort)
          (_, ()) <- waitAny [http', https']
          return ()

