math.randomseed(os.time())

local f = assert(io.open("dataset2.txt", "w"))
local n = 200
local maxW = math.random(1, 10^9)
f:write(string.format("%d %d\n", n, maxW))
for i = 1, n do
  local vi = math.random(1, 10^9)
  local wi = math.random(1, 1000)
  f:write(string.format("%d %d\n", vi, wi))
end
f:close()

local f = assert(io.open("dataset3-1.txt", "w"))
local n = 200
local maxW = math.random(1, 10^9)
f:write(string.format("%d %d\n", n, maxW))
for i = 1, n do
  local vi = math.random(1, 1000)
  local wi = math.random(1, 10^9)
  f:write(string.format("%d %d\n", vi, wi))
end
f:close()

local f = assert(io.open("dataset3-2.txt", "w"))
local n = 200
local maxW = math.random(10^9, 10^9)
f:write(string.format("%d %d\n", n, maxW))
for i = 1, n do
  local vi = math.random(100, 1000)
  local wi = math.random(1, 10^6)
  f:write(string.format("%d %d\n", vi, wi))
end
f:close()

local n = 200
local t,s = {},0
for i = 1, n do
  local vi = math.random(1, 1000)
  local wi = math.random(1, 10^6)
  table.insert(t, {vi, wi})
  s = s + wi
end
local maxW = math.min(10^9, math.ceil(s / 1.7))
local f = assert(io.open("dataset3-3.txt", "w"))
f:write(string.format("%d %d\n", n, maxW))
for i,v in ipairs(t) do
  f:write(string.format("%d %d\n", v[1], v[2]))
end
f:close()
