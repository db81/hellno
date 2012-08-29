{-# LANGUAGE FlexibleContexts #-}

{- |
 Functions to spawn cabal and ghc-pkg.
-}

module Hellno.Cabal where

import System.Process
import System.Exit
import Text.Parsec
import Data.Maybe
import Data.List (intercalate)
import Data.Functor.Identity (Identity)
import Control.Applicative ((<*))

import Hellno
import Hellno.Packages


-- | Do a @cabal install --dry-run@ and collect the package info.
-- If the first argument is True, use --only-dependencies
cabalDryRun :: Bool -> [String] -> IO [PackageId]
cabalDryRun onlyDeps args = do
    let d = if onlyDeps then ["--only-dependencies"] else []
    res <- fmap (parse pkgs "") $ readProcess "cabal" (["install", "--dry-run",
        "--avoid-reinstalls"] ++ d ++ args) ""
    case res of
        (Left err) -> error $ "Parsing cabal output failed: " ++ (show err) -- ?
        (Right a) -> return a


-- | Retrieve the list of currently installed packages as reported by ghc-pkg.
ghcPkgList :: IO [PackageId]
ghcPkgList = do
    res <- fmap (parse pkgs "") $ readProcess "ghc-pkg" ["list"] ""
    case res of
        (Left err) -> error $ "Parsing ghc-pkg output failed: " ++ (show err)
        (Right a) -> return a


-- | Pass the argruments to @cabal install@. Returns True for /ExitSuccess/.
cabalInstall :: [String] -> IO Bool
cabalInstall args = do
    res <- runAndWait "cabal" $ "install":"--only-dependencies":
        "--avoid-reinstalls":"--user":args
    return $ case res of
        (ExitSuccess) -> True
        otherwise -> False


-- | Recache the user package database.
recacheUserDb :: IO ()
recacheUserDb = do
    -- If we don't wait for recaching to finish, it will result in annoying
    -- warnings.
    runAndWait "ghc-pkg" ["recache", "--user"]
    return ()

-- | Take the program name and arguments and wait for it to finish.
runAndWait :: String -> [String] -> IO ExitCode
runAndWait prg args = do
    (_, _, _, h) <- createProcess (proc prg args)
    waitForProcess h


-- | Parse a newline-delimited list of packages skipping lines
-- that can't be parsed as a package name.
pkgs :: (Stream s Identity Char) => Parsec s () [PackageId]
pkgs = fmap catMaybes $ many (try pkg <|> skipLine)
    where skipLine = many (noneOf "\n") >> char '\n' >> return Nothing
          pkg = do
              spaces
              optional $ try $ char '('
              pid <- parsePackageName
              optional $ try $ char ')'
              char '\n'
              return $ Just pid
