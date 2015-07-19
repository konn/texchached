{-# LANGUAGE ExtendedDefaultRules, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes                                         #-}
module Main where
import           Control.Applicative           ((<$>), (<|>))
import           Control.Concurrent
import           Control.Concurrent.QSem
import           Control.Concurrent.STM.Map    (empty)
import qualified Control.Concurrent.STM.Map    as TMap
import           Control.Exception.Lifted
import           Control.Monad.IO.Class
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HM
import           Data.Monoid                   ((<>))
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Text.Lazy                (Text)
import qualified Data.Text.Lazy                as LT
import           Database.Redis                (ConnectInfo (..), PortID (..),
                                                connect, defaultConnectInfo,
                                                runRedis)
import qualified Database.Redis                as Red
import           Network.Wai
import           Shelly.Lifted                 (cmd, shelly, silently)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              (docTypeHtml)
import           Text.Hamlet
import           Web.Scotty

default (T.Text)

redis = defaultConnectInfo {connectHost = "localhost"
                           ,connectPort = PortNumber 6379
                           }

withQSem qsem = bracket_ (liftIO $ waitQSem qsem) (liftIO $ signalQSem qsem)

main :: IO ()
main = do
  con <- connect redis
  qsem <- newQSem 3
  scotty 4049 $ do
    get "/tex.svg" $ do
      src <- param "tex"
      inl <- param "mode" `rescue` const (return "display")
      setHeader "Content-Type" "image/svg+xml"
      let key | inlined = "inline/" <> T.encodeUtf8 src
              | otherwise = "display/" <> T.encodeUtf8 src
          inlined = T.toLower inl == "inline"
      svg <- liftIO $ Red.runRedis con $ Red.get key >>= \case
        Right (Just txt) -> return $ return $ T.decodeUtf8 txt
        _ -> do
          svg <- shelly (withQSem qsem $ silently $ if inlined
                                    then cmd "tex2svg" "--inline" src
                                    else cmd "tex2svg" src)
          if T.null svg
            then return next
            else do
              Red.set key (T.encodeUtf8 svg)
              return $ return svg
      text . LT.fromStrict =<< svg
    get "/" $ do
      html $ renderHtml $ docTypeHtml [shamlet|
          <head>
            <title>tex</title>
          <body>
            <h1>hay
            <p>
              <form method="get" action="tex.svg">
                <input style="width: 120ex;" type="text" #tex name="tex">
                <button>Submit
  |]
  return ()

