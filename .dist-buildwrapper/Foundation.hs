{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Foundation where

import Data.Text (Text, pack)
import Control.Concurrent.STM
import Data.ByteString.Internal (ByteString)
import qualified Data.Text as Text
import Data.Default
import Text.Hamlet
import Yesod.Default.Util
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Either

import Database.MongoDB

import Yesod

data StoredFile = StoredFile !Text !Text !ByteString
type Store = IM.IntMap StoredFile
data App = App Pipe

instance Yesod App where
  shouldLog _ _ _ = True
  defaultLayout widget = do
    pc <- widgetToPageContent $ $(widgetFileNoReload def "default-layout")
    giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

mkYesodData "App" $(parseRoutesFile "config/routes")

fsdb p = access p master "fileserver"

allFilesQuery = rest =<< find (select [] "files") {sort = ["filename" =: 1]}

doc2storedFile :: Document -> StoredFile
doc2storedFile [] = StoredFile "This should never happen" "" ""
doc2storedFile doc = 
  StoredFile fname mimetype content
  where
    fname = fromMaybe "" $ doc !? "filename"
    mimetype = fromMaybe "" $ doc !? "mime"
    bcontent = fromMaybe (Binary "") $ doc !? "content"
    content = bin2bs bcontent
    
bin2bs :: Database.MongoDB.Binary -> ByteString
bin2bs (Database.MongoDB.Binary b) = b
    
storedFile2Doc :: StoredFile -> Document    
storedFile2Doc (StoredFile name mime contents) =
  ["filename" =: name, "mime" =: mime, "content" =: (Binary contents)]    

getList :: Handler [(String, StoredFile)]
getList = do
  App dbh <- getYesod
  allFiles <- fsdb dbh allFilesQuery
  case allFiles of
    Left _ -> notFound
    Right files -> do
      return $ map (\doc -> (show $ (valueAt "_id" doc) , doc2storedFile doc )) files
  
addFile :: App -> StoredFile -> Handler ()
addFile app@(App dbh) file = do
  fsdb dbh $ Database.MongoDB.insert "files" $ storedFile2Doc file
  return ()
    
getByIdQuery id = findOne (select ["_id" =: (read id :: ObjectId)] "files")
    
getById :: String -> Handler StoredFile
getById id = do
  App dbh <- getYesod
  $(logDebug) $ pack $ "_id: "++id
  file <- fsdb dbh $ getByIdQuery id
  $(logDebug) $ pack $ show file
  case file of
    Left _ -> notFound
    Right file -> case file of
                  Nothing -> notFound
                  Just f -> return $ doc2storedFile f