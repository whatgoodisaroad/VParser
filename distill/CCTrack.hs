import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import CCLib (VText, distill, dimensions, latest, ppVText, ccParser)

errorIf :: Bool -> String -> IO ()
errorIf b m = if b then error m else return ()

nextDimensionS :: VText -> String
nextDimensionS vt = case dimensions vt of
  [] -> "A"
  ds -> let d = maximum ds in if (last d) == 'Z' 
      then take (succ $ length d) (repeat 'A')
      else (init d) ++ [succ $ last d]

main :: IO ()
main = do
  args <- getArgs
  let file = head args
  exists <- doesFileExist file
  let target = file ++ ".v"
  vexists <- doesFileExist target
  
  errorIf (length args /= 1) "Usage: CCTrack <filename>"
  errorIf (not exists) $ file ++ " doesnt exist"

  source <- readFile file

  if not vexists 

    -- Create the log file.
    then writeFile target source

    -- Incorporate into the log file.
    else do
      vsource <- readFile target
      let e_vtext = ccParser vsource
      let v_parsed = case e_vtext of { Left _ -> False; Right _ -> True } 
      
      errorIf (not v_parsed) $ "Failed to parse " ++ target

      let Right vtext = e_vtext

      writeFile target 
        $ ppVText 
        $ distill 
            (nextDimensionS vtext) 
            vtext 
            (latest vtext) 
            source
