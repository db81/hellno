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
import Control.Applicative ((<*), (<$>), (<*>))
import Control.Monad
import Control.Exception (throw, throwIO)

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
        (IPI.ParseFailed err) -> throwIO $ userError $ show err


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
    let head' [] = throw $ userError $ "Package " ++ name ++
            " not found in GHC database"
        head' (a:_) = a
    fmap (dropExtension . head' . filter (name`isPrefixOf`)) $
        getDirectoryContents ghcPkg


-- * Managing packages


{- |
 \"grab\" a package that has just been installed by cabal-install
 and put it in the hellno package storage. After that you will probably
 want to \"push\" the package which would create appropriate symlinks in
 ~/.ghc and your cabal installation directory so that the package can be used.
 Returns the /InstalledPackageId/ of the newly grabbed package.
 The second argument is the names of executables provided by the package.
 The package must have a library.
-}
grabPackage :: PackageIdentifier -> [String] -> IO InstalledPackageId
grabPackage pid execs = do
    let (name, fullname) = packageIdToString pid
    hashname <- getHashName pid
    let path = pkgRoot </> name </> fullname </> hashname
    let moveStuff a = do
            e <- doesDirectoryExist $ instPrefix </> a </> fullname
            when e $ moveRecursive (instPrefix </> a </> fullname)
                (path </> a </> fullname)
    moveStuff "lib"
    moveStuff "share"
    moveStuff $ "share" </> "doc"
    moveRecursive (ghcPkg </> hashname <.> "conf") (path <.> "conf")
    unless (null execs) $ createDirectoryIfMissing True $ path </> "bin"
    forM_ execs $ \a' -> do
        let a = "bin" </> a'
        e <- doesFileExist $ instPrefix </> a
        when e $ moveRecursive (instPrefix </> a) (path </> a)
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
    let makeLink name = linkIfExists (path </> name </> fullname)
            (instPrefix </> name </> fullname)
    makeLink "lib"
    makeLink "share"
    makeLink $ "share" </> "doc"
    linkIfExists (path <.> "conf") $ ghcPkg </> hashname <.> "conf"
    bin <- doesDirectoryExist $ path </> "bin"
    when bin $ getDirectoryContents' (path </> "bin") >>=
        mapM_ (\a -> createSymbolicLink (path </> "bin" </> a)
            (instPrefix </> "bin" </> a))
    where linkIfExists dest path = do
            e <- (||) <$> doesDirectoryExist dest <*> doesFileExist dest
            when e $ createSymbolicLink dest path


{- |
 Remove the package's symlinks hereby effectively removing it from GHC database.
-}
pullPackage :: PackageIdentifier -> IO ()
pullPackage pid = do
    let (name, fullname) = packageIdToString pid
    let removeLink a = do
            let path = instPrefix </> a </> fullname
            e <- (||) <$> doesDirectoryExist path <*> doesFileExist path
            when e $ removeFile $ instPrefix </> a </> fullname
    removeLink "lib"
    removeLink "share"
    removeLink $ "share" </> "doc"
    hashname <- getHashName pid
    let binpath = pkgRoot </> name </> fullname </> hashname </> "bin"
    bin <- doesDirectoryExist binpath
    when bin $ getDirectoryContents' binpath >>=
        mapM_ (\a -> removeFile $ instPrefix </> "bin" </> a)
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
     where fromRight (Right a) = a
           fromRight _ = throw $ userError $ "Couldn't parse " ++ str


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
