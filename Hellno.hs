{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{- |
 The main module with stuff used by all others.
-}

module Hellno (
    module Distribution.Package,
    module Distribution.Version,
    instPrefix,
    ghcPkg,
    pkgRoot
    ) where

import Distribution.Package
import Distribution.Version
-- TODO: use this for getting the default user-install prefix. Maybe not.
--import Distribution.Simple.InstallDirs
import Text.Parsec
import Control.Applicative ((<*), (<*>), (<$>))
import System.Directory
import System.FilePath
import System.Process
import System.IO
import System.IO.Unsafe


--instPrefix = "/home/user/haskell"
-- | The user install prefix.
instPrefix :: FilePath
instPrefix = unsafePerformIO $ do
    cnfPath <- fmap (</>"config") $ getAppUserDataDirectory "cabal"
    h <- openFile cnfPath ReadMode
    res <- runParserT p () cnfPath (h, 0 :: Integer)
    hClose h
    case res of
        (Left e) -> error $ show e
        (Right []) -> return $ dropFileName cnfPath
        (Right a) -> return $ last a
    where p = do
            tryTill parserZero skipLine $ string "install-dirs user\n"
            tryTill (do
                skipMany space
                string "prefix:"
                skipMany space
                many (noneOf "\n") <* newline) skipLine $
                string "install-dirs global"
          skipLine = skipMany (noneOf "\n") >> newline

-- Apply parser p or skip using parser skip (should always succeed) until
-- end succeeds, then return the list of all successful invocations of p.
-- Success is mentioned so much in this comment it sounds like an ad for
-- a get-rich-quick scheme. Scheme. Get it?.. Alright, alright, won't do again.
tryTill p skip end =
    let r = return []
     in (eof >> r) <|> (try end >> r) <|> (++) <$>
            (fmap (:[]) (try p) <|> (skip >> r)) <*> tryTill p skip end

--pkgRoot = "/home/user/.cabal/hellno"
-- | The place where hellno will store its stuff.
pkgRoot :: FilePath
pkgRoot = unsafePerformIO $ (</>) <$> getAppUserDataDirectory "cabal" <*>
    return "hellno"

--ghcPkg = "/home/user/.ghc/i386-linux-7.4.1/package.conf.d"
-- | The directory where GHC stores its .conf files for the user package db.
ghcPkg :: FilePath
ghcPkg = unsafePerformIO $ do
    r <- readProcess "ghc-pkg" ["list"] ""
    case parse p "" r of
        (Left e) -> error $ "Parsing ghc-pkg output failed: " ++ show e
        (Right []) -> error "No paths found in ghc-pkg output"
        (Right a) -> return $ last a
    where p = try path <|> try skipLine <|> (eof >> return [])
          skipLine = many (noneOf "\n") >> newline >> p
          path = do
            a <- (:) <$> char '/' <*> many (noneOf ":\n")
            string ":\n"
            fmap (a:) p

-- Might seem like an overkill but it seems better than using readFile and
-- the standard Stream String m Char and then forcing the result. It's possible
-- too, but easier to accidentally mess up.
-- Notice how this instance has to preserve the position inside the file. That's
-- because try's backtracking won't work otherwise which is not entirely obvious
-- from the documentation. Or rather, actually entirely obvious but only after
-- you have figured it out.
instance Stream (Handle, Integer) IO Char where
    uncons (h, pos) = do
        end <- hIsEOF h
        if end then
            return Nothing
        else do
            hSeek h AbsoluteSeek pos
            c <- hGetChar h
            return $ Just (c, (h, pos + 1))
