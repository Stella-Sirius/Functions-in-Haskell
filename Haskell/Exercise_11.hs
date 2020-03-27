module Exercise_11 where

import Control.Monad
import Data.List

type Name = String
type Data = String
data FSItem = File Name Data | Directory Name [FSItem]
  deriving (Eq, Show)

pretty :: FSItem -> String
pretty (File fn da) = concat [fn, " ", "\"", da, "\""] 
pretty (Directory name files) = if null name 
                                then 
                                  if null files 
                                    then concat [name, "/\n"]
                                    else concat [name, "/\n", intercalate "\n" (map notAppendWhiteSpace ["|- " ++ pretty f|f<-sortFileSystem files]), "\n"]
                                else 
                                  if null files
                                  then concat [name, "/\n"]
                                  else concat [name, "/\n", intercalate "\n" (map appendWhiteSpace ["|- " ++ pretty f |f<-sortFileSystem files]), "\n"]

appendWhiteSpace :: String -> String
appendWhiteSpace str = let strs = lines str in (intercalate "\n" $ map ("  " ++ ) strs)

notAppendWhiteSpace :: String -> String
notAppendWhiteSpace str = let strs = lines str in intercalate "\n" strs

sortFileSystem :: [FSItem] -> [FSItem]
sortFileSystem fsItems = getFSWithName fsItems dirNames fileNames
  where
    dirNames = sort $ getNames dirs
    fileNames = sort $ getNames files
    (files, dirs) = getDirAndFile fsItems
    getDirAndFile :: [FSItem] -> ([FSItem],[FSItem])
    getDirAndFile [] = ([],[])
    getDirAndFile (f1:fs1) = let res = getDirAndFile fs1 in
                              case f1 of
                              File name _ -> (f1:fst res, snd res)
                              Directory name _ -> (fst res, f1:snd res)
    getNames :: [FSItem] -> [String]
    getNames [] = []
    getNames ((File name _):fs2) = name:getNames fs2
    getNames ((Directory name _):fs2) = name:getNames fs2
    getFSWithName :: [FSItem] -> [String] -> [String] -> [FSItem]
    getFSWithName _ [] [] = []
    getFSWithName (f3:fs3) [] fNs@(fN:fNss) = case f3 of
                                              File name _ -> if fN == name then f3:getFSWithName fsItems [] fNss else getFSWithName fs3 [] fNs
                                              Directory name _ -> getFSWithName fs3 [] fNs
    getFSWithName (f3:fs3) dNs@(dN:dNss) fNs  = case f3 of
                                              File name _ -> getFSWithName fs3 dNs fNs
                                              Directory name _ -> if dN == name then f3:getFSWithName fsItems dNss fNs else getFSWithName fs3 dNs fNs

main :: IO()
main = do
  let fs = Directory "" []
  request fs

request :: FSItem -> IO()
request fs = do
  userRequest <- getLine
  if userRequest /= "quit"
  then
    do
      let command = words userRequest
      if length command > 1
      then
        do
          let path = head $ tail command
          let modeStr = head command
          let content = if length command > 2 then intercalate " " (tail $ tail command) else ""
          let modFS = doSth fs modeStr path content 
          putStrLn $ pretty modFS
          request modFS
      else
        request fs
  else
    putStrLn "Bye!"

doSth :: FSItem -> String -> String -> String -> FSItem
doSth fsItem@(Directory name fsItems) modeStr path content = if '/' /= head path then fsItem else Directory name (doSth' fsItems modPath)
  where
    isDir = '/' == last path
    modPath = if isDir then init $ tail path else tail path
    trueContent = '\"' == head content && '\"' == last content
    doSth' :: [FSItem] -> String -> [FSItem]
    doSth' [] currentPath = case modeStr of
                            "mkdir" -> if isDir || (not $ '/' `elem` currentPath) then [Directory currentPath []] else []
                            "touch" -> if not $ '/' `elem` currentPath then [File currentPath ""] else []
                            _ -> []
    doSth' (f:fs) currentPath
      | not $ '/' `elem` currentPath = case f of
                                        File name _ -> if name == currentPath 
                                                        then 
                                                          case modeStr of 
                                                            "touch" -> f:fs
                                                            "rm" -> if not isDir then fs else f:doSth' fs currentPath
                                                            "edit" -> if trueContent then (File currentPath (tail $ init content)):fs else f:fs
                                                            _ -> f:doSth' fs currentPath
                                                        else 
                                                          f:doSth' fs currentPath
                                        Directory name _ -> if name == currentPath
                                                              then
                                                                case modeStr of
                                                                  "mkdir" -> f:fs
                                                                  "rm" -> if isDir then fs else f:doSth' fs currentPath
                                                                  _ -> f:doSth' fs currentPath
                                                              else 
                                                                  f:doSth' fs currentPath
      | otherwise = case f of
                    File _ _ -> f:doSth' fs currentPath
                    Directory name fss -> if name == dirName then Directory name (doSth' fss (tail pathLeft)):fs else f:doSth' fs currentPath
      where
        Just i = '/' `elemIndex` currentPath
        (dirName, pathLeft) = splitAt i currentPath