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
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Yaml
import           Options.Applicative
import           System.Environment
import           System.Exit
import           System.IO.Strict
import           System.Process
import           Text.Read

data BumpType = BumpTypeOther Int
              | BumpTypePatch
              | BumpTypeMinor
              | BumpTypeMajor
    deriving(Show, Eq)

data Package = Package String

bumpPackage :: BumpType -> IO (Either String String)
bumpPackage bt = do
    !pkg <- lines <$> readFile "package.yaml"
    let i = findIndex ("version" `isPrefixOf`) pkg
    case i of
        Nothing -> return (Left "No `version` to bump")
        Just i -> do
            let (p, (versionStr:ps)) = splitAt i pkg -- Partial, but can't be
                ev = decodeEither (ByteString.pack versionStr) :: Either String Value
                vstring = ev ^. _Right . key "version" . _String
                vstringS = map Text.unpack (Text.split (== '.') vstring)
                ebv = intercalate "." <$> bump bt vstringS
            case ebv of
                Left e -> return (Left e)
                Right bv -> do
                    writeFile "package.yaml"
                        (unlines (p <> [ByteString.unpack (encode (object ["version" .= bv]))] <> ps))
                    return (Right bv)

bump :: BumpType -> [String] -> Either String [String]
bump BumpTypeMajor (n:ns) = (:(map (const "0") ns)) . show . (+1) <$> readEither n
bump BumpTypeMajor _ = Left "Can't major bump"
bump BumpTypeMinor (n1:n:ns) = (\x -> n1:x:(map (const "0") ns)) . show . (+1) <$> readEither n
bump BumpTypeMinor _ = Left "Can't minor bump"
bump BumpTypePatch (n1:n2:n:ns) = (\x -> n1:n2:x:(map (const "0") ns)) . show . (+1) <$> readEither n
bump BumpTypePatch _ = Left "Can't patch bump"
bump (BumpTypeOther c) ns = if c >= length ns
    then Left ("Can't bump " <> show c <> " component")
    else let (n1, (n:n2)) = splitAt c ns
         in (\x -> n1 <> (x:map (const "0") n2)) . show . (+1) <$> readEither n

readBumpType :: [String] -> Either String BumpType
readBumpType as = case as of
    ("other":[]) -> Left "Usage: stack-bump other <n>"
    ("other":x:_) -> BumpTypeOther <$> readEither x
    ("patch":_) -> Right BumpTypePatch
    ("minor":_) -> Right BumpTypeMinor
    ("major":_) -> Right BumpTypeMajor
    _ -> Left "Usage: stack-bump <patch|minor|major|other <n>>"

run :: BumpType -> IO ()
run bt = do
    ev <- bumpPackage bt
    case ev of
        Left e -> error e
        Right v -> do
            callCommand ("git add package.yaml")
            callCommand ("hpack")
            callCommand ("git add *.cabal")
            callCommand ("git commit -m \"v" <> v <> "\"")
            callCommand ("git tag v" <> v)

            putStrLn ("Bumped version to: " <> v)

main :: IO ()
main = do
    as <- getArgs

    when (listToMaybe as == Just "help") $ do
        putStrLn "Usage: stack-bump <patch|minor|major|other <n>>"
        exitSuccess

    case readBumpType as of
        Left err -> error err
        Right bumpType -> run bumpType
