{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Data.Conduit
import Data.Conduit.Binary
import Data.Default
import Yesod
import Yesod.Default.Util

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Foundation

getHomeR :: Handler Html
getHomeR = do
  (formWidget, formEncType) <- generateFormPost uploadForm
  storedFiles <- getList
  --let storedFiles = []
  defaultLayout $ do
    setTitle "File Processor"
    $(widgetFileNoReload def "home")
        
postHomeR :: Handler Html
postHomeR = do
    ((result, _), _) <- runFormPost uploadForm
    case result of
      FormSuccess fi -> do
        app <- getYesod
        fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
        addFile app $ StoredFile (fileName fi) (fileContentType fi) (toStrict1 fileBytes)
      _ -> return ()
    redirect HomeR

--convert a lazy bytestring to strict
toStrict1 :: BL.ByteString -> B.ByteString
toStrict1 = B.concat . BL.toChunks

uploadForm = renderDivs $ fileAFormReq "file"