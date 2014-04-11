{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Handler.Preview where

import Control.Exception hiding (Handler)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Encoding as TE
import Text.Blaze

import Yesod
import Yesod.Default.Util

import Foundation

getPreviewR :: String -> Handler Html
getPreviewR ident = do
    StoredFile filename _ bytes <- getById ident
    defaultLayout $ do
        setTitle . toMarkup $ "File Processor - " `Text.append` filename
        previewBlock <- liftIO $ preview bytes
        $(widgetFileNoReload def "preview")

preview :: B.ByteString -> IO Widget
preview bytes = do
    eText <- try . evaluate $ TE.decodeUtf8 bytes :: IO (Either SomeException Text.Text)
    return $ case eText of
      Left _ -> errorMessage
      Right text -> [whamlet|<pre>#{text}|]
  where
    errorMessage = [whamlet|<pre>Unable to display file contents.|]