module Main (main) where

import           Control.Monad            (when)
import           System.Console.ParseArgs
import           System.Environment       (getArgs)

import           Fibonacci                (fibb, fibi, fibp, fibr, fibs, fibt)

--
-- declare program parameters
--
data Options =
          FlagHelp
        | OptionBinet
        | OptionIndex
        | OptionParallel
        | OptionRebecca
        | OptionSequence
        | OptionTraditional
          deriving (Ord, Eq, Show)

argd :: [ Arg Options ]
argd = [
        Arg {
            argIndex = FlagHelp,
            argName  = Just "help",
            argAbbr  = Just 'h',
            argData  = Nothing,
            argDesc  = "Help"
        },
        Arg {
            argIndex = OptionBinet,
            argName  = Just "binet",
            argAbbr  = Just 'b',
            argData  = argDataOptional "int" ArgtypeInt,
            argDesc  = "Generate Fibonacci using Binet formula"
        },
        Arg {
            argIndex = OptionIndex,
            argName  = Just "index",
            argAbbr  = Just 'i',
            argData  = argDataOptional "int" ArgtypeInt,
            argDesc  = "Generate Fibonacci at index"
        },
        Arg {
            argIndex = OptionParallel,
            argName  = Just "parallel",
            argAbbr  = Just 'p',
            argData  = argDataOptional "int" ArgtypeInt,
            argDesc  = "Generate Fibonacci in parallel"
        },
        Arg {
            argIndex = OptionRebecca,
            argName  = Just "rebecca",
            argAbbr  = Just 'r',
            argData  = argDataOptional "int" ArgtypeInt,
            argDesc  = "Generate Fibonacci using code from Rebecca Skinner"
        },
        Arg {
            argIndex = OptionSequence,
            argName  = Just "sequence",
            argAbbr  = Just 's',
            argData  = argDataOptional "int" ArgtypeInt,
            argDesc  = "Generate Fibonacci sequence"
        },
        Arg {
            argIndex = OptionTraditional,
            argName  = Just "traditional",
            argAbbr  = Just 't',
            argData  = argDataOptional "int" ArgtypeInt,
            argDesc  = "Generate Fibonacci using traditional recursion"
        }
       ]

--
-- MAIN
--
main :: IO ()
main = do

  argp <- parseArgsIO ArgsComplete argd
  args <- getArgs

  -- show help if no parameters or help flagged
  when (gotArg argp FlagHelp || null args)
    (putStrLn (argsUsage argp))

  case getArgInt argp OptionBinet of
    Just n  -> print (fibb n)
    Nothing -> return ()

  case getArgInt argp OptionIndex of
    Just n  -> print (fibi n)
    Nothing -> return ()

  case getArgInt argp OptionParallel of
    Just n  -> print (fibp n)
    Nothing -> return ()

  case getArgInt argp OptionRebecca of
    Just n  -> print (fibr n)
    Nothing -> return ()

  case getArgInt argp OptionSequence of
    Just n  -> mapM_ print (take (n + 1) fibs)
    Nothing -> return ()

  case getArgInt argp OptionTraditional  of
    Just n  -> print (fibt n)
    Nothing -> return ()
