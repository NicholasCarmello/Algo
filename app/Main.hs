{-
 -  Main.hs
 -
 -  Reference implementation of the toy language HOPL.LET by Mitchell Wand.
 -  This module provides routines for executables baesd on LET.
 -
 -  Author: Matthew A Johnson
 -}
module Main where

import Control.Exception (ErrorCall)
import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import HOPL.LET.Interp (interp)
import System.Console.Haskeline
import System.Environment (getArgs)
import System.IO (hPrint, stderr)

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "LET> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input ->
          unless (input == ":q") $
            liftIO
              ( case interp input of
                  Left err -> print err
                  Right val -> print val
                  `catch` (\e -> hPrint stderr (e :: ErrorCall))
              )
              >> loop

run :: IO ()
run = do
  args <- getArgs
  if null args
    then putStrLn "hopl3-run: Missing source file name"
    else do
      prog <- readFile $ head args
      case interp prog of
        Left err -> print err
        Right val -> print val
        `catch` (\e -> hPrint stderr (e :: ErrorCall))
      return ()
