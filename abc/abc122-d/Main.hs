import System.Environment
import qualified Small
import qualified Fast
import qualified MatPow
import qualified PolyDiv

main = do
  args <- getArgs
  case args of
    "small":_ -> Small.main
    "matpow":_ -> MatPow.main
    "polydiv":_ -> PolyDiv.main
    _ -> Fast.main
