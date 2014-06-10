--  A simple command-line tool for applying selections to variational code.
--  Takes a selection as command line arguments, and uses it to select 
--  variational code on STDIN to a variant on STDOUT.
--
--  e.g., in Bash:
--    cat MyCode.hs.v | CCSelect X.l Y.r > MyCode.hs
--

import Data.List (intersperse)
import System.Environment (getArgs)

import CCLib

main = do
  sel <- 
    (validateSelection . selParser . concat . intersperse " ") `fmap` getArgs
  case sel of
    (Left m) -> print m
    (Right s) -> do
      input <- getContents
      let vin = ccParser input
      case vin of
        (Left m) -> putStrLn "Couldn't parse input structure"
        (Right i) -> putStr $ concatMap show $ applySelection s i
