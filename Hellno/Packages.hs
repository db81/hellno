{-# LANGUAGE FlexibleContexts #-}

{- |
 Manage hellno's precompiled package database.
-}

module Hellno.Packages where

import qualified Distribution.InstalledPackageInfo as IPI
import System.Directory
import System.FilePath
import System.Posix.Files
import Data.List (intercalate, isPrefixOf)
import Data.Functor.Identity (Identity)
import Text.Parsec
import Control.Applicative ((<*))
import Control.Monad

import Hellno


-- * Lookup functions


-- | Lookup a package in hellno's db.
lookupPackage :: PackageIdentifier -> IO [InstalledPackageId]
lookupPackage pid = do
    let (name, fullname) = packageIdToString pid
    let path = pkgRoot </> name </> fullname
    de <- doesDirectoryExist path
    if not de then
        return []
    else
        fmap (map InstalledPackageId) $ filterM (doesDirectoryExist .
            (path</>)) =<< getDirectoryContents' path

-- | Lookup the dependecies of a package in hellno's db.
lookupPackageDeps :: InstalledPackageId -> IO [InstalledPackageId]
lookupPackageDeps ipid = do
    let (pid, hashname) = parseInstalledPackageId ipid
    let (name, fullname) = packageIdToString pid
    res <- fmap IPI.parseInstalledPackageInfo $ readFile $
        pkgRoot </> name </> fullname </> (hashname ++ ".conf")
    case res of
        (IPI.ParseOk [] a) -> return $ IPI.depends a
        (IPI.ParseFailed err) -> error $ show err -- hmm?


-- | List all packages in the database.
listPackages :: IO [InstalledPackageId]
listPackages =
    fmap (map InstalledPackageId . concat . concat) $
        getDirectoryContents' pkgRoot >>= mapM (\p ->
            getDirectoryContents' (pkgRoot </> p) >>= mapM (\v ->
                getDirectoryContents' (pkgRoot </> p </> v) >>=
                    filterM (doesDirectoryExist . ((pkgRoot </> p </> v) </>))))


-- | Get the current .conf file corresponding to the package
-- (without the \".conf\" part).
getHashName :: PackageIdentifier -> IO String
getHashName pid = do
    let (_, name) = packageIdToString pid
    fmap (dropExtension . head . filter (name`isPrefixOf`)) $ -- head is unsafe!
        getDirectoryContents ghcPkg


-- * Managing packages


{- |
 \"grab\" a package that has just been installed by cabal-install
 and put it in the hellno package storage. After that you will probably
 want to \"push\" the package which would create appropriate symlinks in
 ~/.ghc and your cabal installation directory so that the package can be used.
 Returns the /InstalledPackageId/ of the newly grabbed package.
-}
grabPackage :: PackageIdentifier -> IO InstalledPackageId
grabPackage pid = do
    let (name, fullname) = packageIdToString pid
    hashname <- getHashName pid
    -- We won't touch share/ for now.
    moveRecursive (instPrefix </> "lib" </> fullname) $ pkgRoot </> name </>
        fullname </> hashname
    moveRecursive (ghcPkg </> (hashname ++ ".conf")) $ pkgRoot </> name </>
        fullname </> (hashname ++ ".conf")
    return $ InstalledPackageId hashname


{- |
 Create symlinks to the package \"grabbed\" by hellno in ~/.ghc and the cabal
 installation directory.
-}
pushPackage :: InstalledPackageId -> IO ()
pushPackage ipid = do
    let (pid, hashname) = parseInstalledPackageId ipid
    let (name, fullname) = packageIdToString pid
    let path = pkgRoot </> name </> fullname </> hashname
    createSymbolicLink path $ instPrefix </> "lib" </> fullname
    createSymbolicLink (path ++ ".conf") $ ghcPkg </> (hashname ++ ".conf")


{- |
 Remove the package's symlinks hereby effectively removing it from GHC database.
-}
pullPackage :: PackageIdentifier -> IO ()
pullPackage pid = do
    let (_, fullname) = packageIdToString pid
    hashname <- getHashName pid
    removeFile $ instPrefix </> "lib" </> fullname
    removeFile $ ghcPkg </> (hashname ++ ".conf")


-- | Removes the package from hellno's database.
dropPackage :: InstalledPackageId -> IO ()
dropPackage ipid = do
    let (pid, hashname) = parseInstalledPackageId ipid
        (name, fullname) = packageIdToString pid
    deleteRecursive $ pkgRoot </> name </> fullname </> hashname
    removeFile $ pkgRoot </> name </> fullname </> (hashname ++ ".conf")
    deleteIfEmpty $ pkgRoot </> name </> fullname
    deleteIfEmpty $ pkgRoot </> name
    where deleteIfEmpty dir = do
            e <- fmap null $ getDirectoryContents' dir
            when e $ removeDirectory dir


-- | \"Pull\" all packages managed by hellno.
clearPackages :: IO ()
clearPackages = getDirectoryContents' ghcPkg >>= filterM islnk >>=
    mapM_ (pullPackage . fst . parseInstalledPackageId . InstalledPackageId)
    where islnk f = fmap isSymbolicLink $ getSymbolicLinkStatus $ ghcPkg </> f


-- * Parsing/showing


-- | Converts a package identifier to a tuple of (\"foo\", \"foo-0.2.1\")
packageIdToString :: PackageIdentifier -> (String, String)
packageIdToString (PackageIdentifier (PackageName name) (Version ver _)) =
    (name, name ++ "-" ++ intercalate "." (map show ver))


-- | Takes input of the form \"foo-1.2.3\".
parsePackageName :: (Stream s Identity Char) => Parsec s () PackageId
parsePackageName = do
    name <- fmap (intercalate "-") $ many $ try $
        many (noneOf "-.\n") <* char '-'
    ver <- fmap (map read) $ sepBy1 (many1 digit) (char '.')
    return $ PackageIdentifier (PackageName name) (Version ver [])


parseInstalledPackageId :: InstalledPackageId -> (PackageId, String)
parseInstalledPackageId (InstalledPackageId str) =
     (fromRight $ parse parsePackageName "" str, str)
     where fromRight (Right a) = a -- TODO: fix this


-- * Moving directories around


moveRecursive :: FilePath -> FilePath -> IO ()
moveRecursive path dest = copyRecursive path dest >> deleteRecursive path


copyRecursive :: FilePath -> FilePath -> IO ()
copyRecursive path dest = do
    fs <- getSymbolicLinkStatus path
    case (isDirectory fs, isSymbolicLink fs) of
        (True, False) -> do
            createDirectoryIfMissing True dest
            getDirectoryContents' path >>=
                mapM_ (\p -> copyRecursive (path</>p) (dest</>p))
        (False, True) -> readSymbolicLink path >>=
            (flip createSymbolicLink) dest
        otherwise -> copyFile path dest


deleteRecursive :: FilePath -> IO ()
deleteRecursive path = do
    fs <- getSymbolicLinkStatus path
    if isDirectory fs then do
        getDirectoryContents' path >>=
            mapM_ (deleteRecursive . (path</>))
        removeDirectory path
    else
        removeFile path


getDirectoryContents' a = fmap (filter (`notElem` [".",".."])) $
    getDirectoryContents a
