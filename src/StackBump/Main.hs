{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module StackBump.Main where

import           Prelude               hiding (readFile)

import           Control.Lens          hiding ((.=))
import           Control.Monad
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8 as ByteString (pack, unpack)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text             as Text
import           Data.Yaml
import           System.Console.ANSI
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.FilePath.Glob
import           System.IO.Strict
import           Text.Read

import           StackBump.Spinner

data BumpType
  = BumpTypeOther Int
  | BumpTypePatch
  | BumpTypeMinor
  | BumpTypeMajor
  deriving (Show, Eq)

data Package =
  Package String

bumpPackage :: BumpType -> IO (Either String (String, String))
bumpPackage bt = do
  !pkg <- lines <$> readFile "package.yaml"
  let mi = findIndex ("version" `isPrefixOf`) pkg
  case mi of
    Nothing -> return (Left "No `version` to bump")
    Just i -> do
      let (p, versionStr:ps) = splitAt i pkg -- Partial, but can't be
          ev = decodeEither (ByteString.pack versionStr) :: Either String Value
          vstring = ev ^. _Right . key "version" . _String
          vstringS = map Text.unpack (Text.split (== '.') vstring)
          ebv = intercalate "." <$> bump bt vstringS
      case ebv of
        Left e -> return (Left e)
        Right bv -> do
          let packageYaml' =
                unlines
                  (p <>
                   [ init
                       (ByteString.unpack (encode (object ["version" .= bv])))
                   ] <>
                   ps)
          return (Right (packageYaml', bv))

bump :: BumpType -> [String] -> Either String [String]
bump BumpTypeMajor (n:ns) =
  (: map (const "0") ns) . show . (+ (1 :: Int)) <$> readEither n
bump BumpTypeMajor _ = Left "Can't major bump"
bump BumpTypeMinor (n1:n:ns) = (\x -> n1:x:map (const "0") ns) . show . (+(1 :: Int)) <$> readEither n
bump BumpTypeMinor _ = Left "Can't minor bump"
bump BumpTypePatch (n1:n2:n:ns) =
  (\x -> n1 : n2 : x : map (const "0") ns) . show . (+ (1 :: Int)) <$>
  readEither n
bump BumpTypePatch _ = Left "Can't patch bump"
bump (BumpTypeOther c) ns =
  if c >= length ns
    then Left ("Can't bump " <> show c <> " component")
    else let (n1, n:n2) = splitAt c ns
         in (\x -> n1 <> (x : map (const "0") n2)) . show . (+ (1 :: Int)) <$>
            readEither n

readBumpType :: [String] -> Either String BumpType
readBumpType as =
  case as of
    ["other"]     -> Left "Usage: stack-bump other <n>"
    ("other":x:_) -> BumpTypeOther <$> readEither x
    ("patch":_)   -> Right BumpTypePatch
    ("minor":_)   -> Right BumpTypeMinor
    ("major":_)   -> Right BumpTypeMajor
    _             -> Left "Usage: stack-bump <patch|minor|major|other <n>>"

runTasks :: String -> IO a -> IO ()
runTasks title action = do
  setSGR [SetColor Foreground Vivid Yellow]
  putChar '•'
  setSGR [SetColor Foreground Vivid Black]
  putStrLn (" " <> title)
  _ <- action
  cursorUp 1
  clearLine
  setCursorColumn 0
  setSGR [SetColor Foreground Vivid Green]
  putChar '✓'
  setSGR [SetColor Foreground Vivid Black]
  putStrLn (" " <> title)

run :: BumpType -> IO ()
run bt = do
  ev <- bumpPackage bt
  case ev of
    Left e -> error e
    Right (packageYaml', v) -> do
      runTasks "Checking if package is good for publishing" $ do
        runProcessWithSpinner "stack build"
        runProcessWithSpinner "stack test"
        runProcessWithSpinner "stack sdist"
      runTasks ("Writting new version (v" <> v <> ")") $
        writeFile "package.yaml" packageYaml'
      runTasks ("Commiting (v" <> v <> ")") $ do
        runProcessWithSpinner "stack build"
        runProcessWithSpinner "git add package.yaml"
        mcabalFile <- findCabalfile
        case mcabalFile of
          Just cabalFile -> runProcessWithSpinner ("git add " <> cabalFile)
          Nothing        -> return ()
        runProcessWithSpinnerRaw
          "git commit -m ..."
          "git"
          ["commit", "-m", "Bump version to v" <> v]
        runProcessWithSpinner ("git tag v" <> v)
      putStrLn ""
      setSGR [SetColor Foreground Vivid White]
      putStrLn ("Bumped version to: v" <> v)

findCabalfile :: IO (Maybe FilePath)
findCabalfile = do
  mgitIgnore <- listToMaybe <$> glob ".gitignore"
  case mgitIgnore of
    Nothing -> findCabalfile'
    Just gitIgnore -> do
      gitIgnoreC <- lines <$> readFile gitIgnore
      let ignoringCabalFile =
            isJust $ find ((== ".cabal") . takeExtension) gitIgnoreC
      if ignoringCabalFile
        then return Nothing
        else findCabalfile'
  where
    findCabalfile' = listToMaybe <$> glob "./*.cabal"

main :: IO ()
main = do
  as <- getArgs
  when (listToMaybe as == Just "help") $ do
    putStrLn "Usage: stack-bump <patch|minor|major|other <n>>"
    exitSuccess
  case readBumpType as of
    Left err       -> error err
    Right bumpType -> run bumpType
