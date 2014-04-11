{-# LANGUAGE OverloadedStrings #-}

module Handler.Download where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Yesod

import Foundation

getDownloadR :: String -> Handler TypedContent
getDownloadR id = do
  StoredFile filename contentType bytes <- getById id
  addHeader "Content-Disposition" $ Text.concat
    ["attachment; filename=\"",filename,"\""]
  sendResponse (Text.encodeUtf8 contentType, toContent bytes)