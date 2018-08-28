module Main where

import           Control.Monad            (when)
import           System.Console.ParseArgs

import           Fibonacci                (fibi, fibp, fibs)

--
-- declare program parameters
--
data Options =
          FlagHelp
        | OptionIndex
        | OptionSequence
        | OptionParallel
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
            argIndex = OptionIndex,
            argName  = Just "index",
            argAbbr  = Just 'i',
            argData  = argDataOptional "int" ArgtypeInt,
            argDesc  = "Generate Fibonacci at index (>0)"
        },
        Arg {
            argIndex = OptionSequence,
            argName  = Just "sequence",
            argAbbr  = Just 's',
            argData  = argDataOptional "int" ArgtypeInt,
            argDesc  = "Generate Fibonacci sequence (>0)"
        },
        Arg {
            argIndex = OptionParallel,
            argName  = Just "parallel",
            argAbbr  = Just 'p',
            argData  = argDataOptional "int" ArgtypeInt,
            argDesc  = "Generate Fibonacci value in parallel (>0)"
        }
       ]

--
-- MAIN
--
main :: IO ()
main = do

  args <- parseArgsIO ArgsComplete argd

  when (gotArg args FlagHelp)
    (
      putStrLn (argsUsage args)
    )

  case getArgInt args OptionIndex of
    Just n  -> print (fibi n)
    Nothing -> return ()

  case getArgInt args OptionSequence of
    Just n  -> print (take n fibs)
    Nothing -> return ()

  case getArgInt args OptionParallel of
    Just n  -> print (fibp n)
    Nothing -> return ()

