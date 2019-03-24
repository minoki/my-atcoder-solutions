import System.Environment
import qualified Small
import qualified Fast

main = do
  args <- getArgs
  case args of
    "small":_ -> Small.main
    _ -> Fast.main
