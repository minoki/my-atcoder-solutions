#!/usr/bin/env lua

if arg[1] == nil or arg[1] == "--help" or arg[1] == "-h" then
  io.stderr:write(string.format("%s <contest-name>\n", arg[0]))
  os.exit(1)
end

local name = arg[1]

os.execute("mkdir " .. name) -- Ignore error

local filename = name .. "/Main.hs"
local fh = io.open(filename, "r")
if fh then
  fh:close()
  io.stderr:write(string.format("%s already exists.\n", filename))
  os.exit(1)
end
fh = assert(io.open(filename, "w"))
fh:write([[
-- https://github.com/minoki/my-atcoder-solutions
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (unfoldr)
import Control.Monad
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as BS

main = do
  _ :: [Int] <- map (read . BS.unpack) . BS.words <$> BS.getLine
  [n,m] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  edges <- U.replicateM m $ do
    [x,y,z] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
    return (x-1,y-1,z)
  return ()
]])
fh:close()
