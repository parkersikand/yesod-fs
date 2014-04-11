{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Database.MongoDB

allFilesQuery = rest =<< find (select [] "files") {sort = ["filename" =: 1]}

fsdb p = access p master "fileserver"

itemID = "53471f5fd6194c0c42000000"

dbconn = runIOE $ connect (host "127.0.0.1")

getByIdQuery id = findOne (select ["_id" =: (read id :: ObjectId) ] "files")

getByIdQuery2 id = findOne (select ["filename" =: "fastfood.txt"] "files")

testFetch = do
  c <- dbconn
  file <- fsdb c $ getByIdQuery itemID
  putStrLn $ show file
  
testAll = do
  conn <- dbconn
  allFiles <- fsdb conn allFilesQuery
  case allFiles of
    Right files -> do
      putStrLn "valueAt"
      mapM_ (\doc -> do putStrLn $ show $ valueAt "_id" doc) files
      putStrLn "!?"
      mapM_ (\doc -> do putStrLn $ show $ doc !? "_id") files
      putStrLn "at"
      mapM_ (\doc -> do putStrLn $ show $ (at "_id" doc :: ObjectId) ) files
    otherwise -> putStrLn "error"

main = do
  conn <- runIOE $ connect (host "127.0.0.1")
  allFiles <- fsdb conn allFilesQuery
  case allFiles of
    Left _ -> error "Couldn't fetch files"
    Right files@(_:f:fs) -> do
      putStrLn $ "Number of records: " ++ (show $ length files)
      putStrLn $ "Number of fields: " ++ (show $ length f)
      putStrLn $ "_id: " ++ (show $ (f !? "_id") )
      putStrLn $ "_id: " ++ (show $ valueAt "_id" f)
      putStrLn $ "filename: " ++ (show $ (f !? "filename") )
      putStrLn $ show f