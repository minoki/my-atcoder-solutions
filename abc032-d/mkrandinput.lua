math.randomseed(os.time())

local f = assert(io.open("dataset2.txt", "w"))
local n = 100
local maxW = math.random(1, 10^9)
f:write(string.format("%d %d\n", n, maxW))
for i = 1, n do
  local vi = math.random(1, 10^9)
  local wi = math.random(1, 1000)
  f:write(string.format("%d %d\n", vi, wi))
end
f:close()

local f = assert(io.open("dataset3.txt", "w"))
local n = 100
local maxW = math.random(1, 10^9)
f:write(string.format("%d %d\n", n, maxW))
for i = 1, n do
  local vi = math.random(1, 1000)
  local wi = math.random(1, 10^9)
  f:write(string.format("%d %d\n", vi, wi))
end
f:close()
