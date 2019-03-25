import System.Environment
import qualified Small
import qualified Fast
import qualified MatPow

main = do
  args <- getArgs
  case args of
    "small":_ -> Small.main
    "matpow":_ -> MatPow.main
    _ -> Fast.main
