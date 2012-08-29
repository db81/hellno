{- |
 The Main module.
-}

module Main where

import System.Console.GetOpt
import System.Environment
import System.Exit

import Hellno
import Hellno.Packages
import Hellno.Cabal
import Hellno.Core


data Opt = Opt String | ArgOpt String String

instance Eq Opt where
    (==) (Opt s) (Opt s') = s == s'
    (==) (ArgOpt s _) (ArgOpt s' _) = s == s'

simpleOpt :: Char -> String -> String -> OptDescr Opt
simpleOpt c o s = Option [c] [o] (NoArg $ Opt o) s

options = [
    simpleOpt 'h' "help" "display this help message"
    ]

usageHeader = unlines [
    "Usage:",
    "hellno [OPTIONS] < setup | try | depclean > [PACKAGES]",
    "hellno [OPTIONS] reset\n",
    "Modes:",
    "setup - configure the environment for building the given package(s)",
    "try - the same as setup but don't install anything",
    "local-install - install using cabal-src-install",
    "depclean - clean the database so that only packages required by at least",
    "one of the arguments remain",
    "reset - reset the environment so that only fixed packages remain,",
    "useful if you need to do your own cabal install\n",
    "Options:"
    ]

handleArgs :: [Opt] -> [String] -> [String] -> IO ()
handleArgs opts mode err
    | not (null err) = mapM_ putStrLn err >> exitFailure
    | Opt "help" `elem` opts = putStrLn (usageInfo usageHeader options)
    | ("setup":xs) <- mode = setupEnvironment False xs
    | ("try":xs) <- mode = setupEnvironment True xs
    | ("depclean":xs) <- mode = cleanDatabase xs
    | ("local-install":[]) <- mode = cabalSrcInstall
    | ("reset":[]) <- mode = clearPackages >> recacheUserDb
    | otherwise = putStrLn "Error: No mode specified or unknown mode." >>
        exitFailure

main = do
    (opts, mode, err) <- fmap (getOpt Permute options) $ instPrefix `seq`
        ghcPkg `seq` pkgRoot `seq` getArgs
    handleArgs opts mode err
