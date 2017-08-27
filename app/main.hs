module Main where

import           Control.Monad            (when)
import           System.Console.ParseArgs

import           Fibonacci                (fibi, fibs)

--
-- declare program parameters
--
data Options =
        FlagHelp        |
        OptionIndex     |
        OptionSequence
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
            argDesc  = "Generate Fibonacci at index"
        },
        Arg {
            argIndex = OptionSequence,
            argName  = Just "sequence",
            argAbbr  = Just 's',
            argData  = argDataOptional "int" ArgtypeInt,
            argDesc  = "Generate Fibonacci sequence"
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
    Just x  -> putStrLn $ show (fibi x)
    Nothing -> return ()

  case getArgInt args OptionSequence of
    Just x  -> putStrLn $ show (take x fibs)
    Nothing -> return ()

