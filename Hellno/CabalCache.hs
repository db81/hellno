{- |
 Access the Cabal package info cache.
-}

module Hellno.CabalCache (
    Cache,
    readCache,
    getPackageDescriptions,
    getCondExecutables,
    hasCondLibrary
    ) where

import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.Parsec
import System.IO
import System.FilePath
import Control.Monad
import Control.Applicative ((<*), (<*>), (<$>))
import Control.Exception (throw, throwIO)
import Data.Char (ord, chr)
import Numeric

import Hellno
import Hellno.Packages


-- | A data structure used to represent 00-index.cache files. We use
-- ByteStrings to conserve memory.
data Cache = Cache { tarPath :: FilePath, blockMap :: Map ByteString Int }
    deriving (Show)

-- | Read a cache file from the disk.
readCache :: FilePath -> IO Cache
readCache path = do
    h <- openFile path ReadMode
    res <- fmap (runParser p Map.empty path) $ BS.hGetContents h
    case res of
        (Left e) -> throwIO $ userError $ show e
        (Right a) -> return $ Cache (dropExtension path <.> "tar") a
    where p = do
             string "pkg: "
             name <- w <* space
             ver <- w <* space
             string "b# "
             block <- w
             optional newline
             modifyState $ Map.insert
                (str2bs $ concat [name, "-", ver]) (read block)
             p <|> (eof >> getState)
          w = many (noneOf " \n")


-- | Get package descriptions using the provided caches.
getPackageDescriptions :: [Cache] -> [PackageId] -> IO [GenericPackageDescription]
getPackageDescriptions caches pkgs = do
    let ts = map tarPath caches
    tars <- fmap (Map.fromList . zip ts) $ mapM (`openFile` ReadMode) ts
    go tars caches pkgs <* mapM_ hClose (Map.elems tars)
    where go _ _ [] = return []
          go tars caches (p:pkgs) = do
              let (h, b) = find tars caches $ str2bs $
                    snd $ packageIdToString p
              hSeek h AbsoluteSeek $ fromIntegral b * 512 + 124
              s <- fmap (fst . head . readOct . bs2str) $ BS.hGetSome h 11
              hSeek h AbsoluteSeek $ (fromIntegral b + 1) * 512
              res <- fmap (parsePackageDescription . bs2str) $ BS.hGetSome h s
              case res of
                (ParseFailed e) -> throwIO $ userError $
                    "parsePackageDescription: " ++ show h ++ "; b# " ++
                    show b ++ "; " ++ show e
                (ParseOk _ gpd) -> fmap (gpd:) $ go tars caches pkgs
          find _ [] p = throw $ userError $ "Error: not found in cabal " ++
                "package cache: " ++ show p ++
                "\n(consider running cabal update and install local " ++
                "packages using local-install)"
          find tars (c:caches) p =
              case Map.lookup p (blockMap c) of
                    Nothing -> find tars caches p
                    (Just b) -> (tars ! tarPath c, b)

-- | Obtain condExecutables' names from GenericPackageDescription.
getCondExecutables :: GenericPackageDescription -> [String]
getCondExecutables = map fst . condExecutables

-- | Does the package has a library in it?
hasCondLibrary :: GenericPackageDescription -> Bool
hasCondLibrary = cl . condLibrary
    where cl (Just _) = True
          cl (Nothing) = False


bs2str = map (chr . fromIntegral) . BS.unpack
str2bs = BS.pack . map (fromIntegral . ord)
