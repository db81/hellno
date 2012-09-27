{-# LANGUAGE ScopedTypeVariables #-}

{- |
 For lack of a better name, that's the module that deals with the core
 functionality, that is, figuring out what packages to install.

 Since writing a solver is indeed a non-trivial task, we cheat by using
 cabal-install, though this approach has its drawbacks.
-}

module Hellno.Core where

import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad
import qualified Control.Exception as E
import System.FilePath
import System.Directory
import System.IO.Error

import Hellno
import Hellno.Packages
import Hellno.Cabal
import Hellno.CabalCache



-- | A datatype describing what we intend to do with a package.
data PackageResolution
    = Fixed PackageId -- ^ The package is fixed and we can't do anything with it (usually packages in the global db)
    | Pending PackageId -- ^ We're yet to decide what to do with the package.
    | CabalInstall PackageId -- ^ Package to be installed by cabal.
    | Precompiled InstalledPackageId -- ^ We'll use the specific precompiled version.
    deriving (Show)


{- |
 After taking package name(s) as input this function tries to make sure
 all required dependecies are present by either obtaining them from the
 database of precompiled packages or asking cabal to install them.

 It currently uses @cabal install --dry-run@ to determine the dependencies
 and throws an exception if cabal tells us it cannot resolve them.

 If the first argument is True, only push required precompiled packages and
 don't install anything.
-}
setupEnvironment :: Bool -> [String] -> IO ()
setupEnvironment pretend args = do
    clearPackages
    recacheUserDb
    fixed <- ghcPkgList
    req <- cabalDryRun True args
    -- What happens when cabal requires a different version of a fixed package?
    putStrLn $ (show $ length fixed) ++ " fixed packages, " ++
        (show $ length req) ++ " other packages required."
    m <- resolveEnvironment fixed req
    let precomp = map (\(Precompiled i) -> i) $ filter isPrecomp $ M.elems m
    let cabalinst = map (\(CabalInstall i) -> i) $ filter isCabalInst $
                        M.elems m
    putStrLn $ "Using " ++ (show $ length precomp) ++
        " precompiled packages, installing " ++ (show $ length cabalinst)
    mapM_ pushPackage precomp
    recacheUserDb
    unless pretend $ do
        cabalInstall args
        grabAndPush cabalinst
        recacheUserDb

-- | Grab the packages and then push them.
grabAndPush :: [PackageId] -> IO ()
grabAndPush [] = return ()
grabAndPush pkgs = do
    cabal <- fmap ((</> "packages")) $ getAppUserDataDirectory "cabal"
    caches <- mapM (readCache . (\a -> cabal </> a </> "00-index.cache")) =<<
        getDirectoryContents' cabal
    mapM_ pushPackage . catMaybes =<< mapM (uncurry grabPackage') .
        zip pkgs . map getCondExecutables . filter hasCondLibrary =<<
        getPackageDescriptions caches pkgs
    where grabPackage' pid execs = do
            res <- E.try $ grabPackage pid execs
            case res of
                (Right a) -> return $ Just a
                (Left (e :: E.IOException)) -> do
                    putStrLn $ "WARNING: could not grab package " ++
                        snd (packageIdToString pid) ++ ": " ++
                        if ioeGetErrorType e == userErrorType then
                            ioeGetErrorString e
                        else
                            show e
                    return Nothing


-- | Clean the precompiled packages database only leaving the packages
-- that are required by at least one of the arguments.
cleanDatabase :: [String] -> IO ()
cleanDatabase args' = do
    n <- fmap length listPackages
    let args = case args' of
                ([]) -> error $ unlines
                    ["You gave empty package list to depclean. If you want to",
                     "delete the whole database use hellno depclean all\n",
                     show n ++ " packages currently in the database."]
                (["all"]) -> []
                a -> a
    clearPackages
    recacheUserDb
    fixed <- ghcPkgList
    saved <- foldM (\s p -> do
        req <- cabalDryRun True [p]
        fmap (( S.union s . S.fromList . map (\(Precompiled i) -> i) .
            filter isPrecomp . M.elems )) $ resolveEnvironment fixed req)
            S.empty args
    mapM_ (\p -> unless (p `S.member` saved) $ dropPackage p) =<< listPackages
    n' <- fmap length listPackages
    putStrLn $ "Dropped " ++ (show $ n - n') ++ " packages."


-- TODO: Merge these two.
cabalSrcInstall :: IO ()
cabalSrcInstall = do
    setupEnvironment False []
    pkgs <- cabalDryRun False []
    r <- findExecutable "cabal-src-install"
    when (r == Nothing) $ E.throwIO $ userError $
        "Couldn't find cabal-src-install (is it in PATH?)"
    runAndWait "cabal-src-install" ["--user"]
    runAndWait "cabal" ["info"] -- this updates 00-index.cache
    mapM lookupPackage pkgs >>= mapM_ dropPackage . concat
    grabAndPush pkgs
    recacheUserDb

cabalInstallPackage :: [String] -> IO ()
cabalInstallPackage args = do
    setupEnvironment False args
    pkgs <- cabalDryRun False args
    runAndWait "cabal" $ ["--user", "install"] ++ args
    grabAndPush pkgs
    recacheUserDb


-- | Take the list of fixed packages and the list of required packages and
-- return a map describing how to achieve the desired state.
resolveEnvironment :: [PackageId] -> [PackageId] ->
    IO (Map String PackageResolution)
resolveEnvironment fixed req =
    execStateT (resolve $ map pid2str req) $ M.union
        (M.fromList $ map (\a -> (pid2str a, Fixed a)) fixed)
        (M.fromList $ map (\a -> (pid2str a, Pending a)) req)


type ResolvingState a = StateT (Map String PackageResolution) IO a

-- | Run resolveTree for every package that is still pending.
-- Takes the list of all pending packages.
resolve :: [String] -> ResolvingState ()
resolve [] = return ()
resolve (p:ps) = do
    s <- gets (!p)
    case s of
        (Pending pid) -> resolveTree p >> resolve ps
        otherwise -> resolve ps

-- | Go down the dependency tree and decide what to do with the packages.
resolveTree :: String -> ResolvingState Bool
resolveTree name = do
    s' <- gets (M.lookup name)
    case s' of
        Nothing -> return False
        (Just s) -> case s of
            (CabalInstall _) -> return False
            (Pending pid) -> pending pid
            otherwise -> return True
    where pending pid = do
            -- A little explanation of what goes on here: we take the list of
            -- all precompiled variants of the package and take the first one
            -- that passes the test, which consists of checking that all
            -- dependencies are either precompiled too or fixed and then making
            -- sure that packages' hashes exactly match (checkDep).
            l <- liftIO (lookupPackage pid) >>= dropWhileM (\i -> do
                deps <- liftIO $ lookupPackageDeps i
                resv <- fmap (not . and) $ mapM (resolveTree . ipid2str) deps
                right <- fmap (not . and) $ mapM checkDep deps
                return $ resv || right)
            if null l then do
                modify $ M.adjust (const $ CabalInstall pid) name
                return False
            else do
                modify $ M.adjust (const $ Precompiled $ head l) name
                return True
          checkDep dep = do
            d <- gets $ M.lookup $ ipid2str dep
            case d of
                (Just (Fixed _)) -> return True
                (Just (Precompiled i)) -> return $ i == dep
                otherwise -> return False
          ipid2str = pid2str . fst . parseInstalledPackageId


dropWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ [] = return []
dropWhileM f (x:xs) = do
    r <- f x
    if r then dropWhileM f xs
    else return (x:xs)


isPrecomp :: PackageResolution -> Bool
isPrecomp (Precompiled _) = True
isPrecomp _ = False

isCabalInst :: PackageResolution -> Bool
isCabalInst (CabalInstall _) = True
isCabalInst _ = False


-- Almost duplicating Hellno.Packages.packageIdToString...
pid2str :: PackageId -> String
pid2str (PackageIdentifier (PackageName name) (Version vs _)) =
    name ++ "-" ++ intercalate "." (map show vs)
