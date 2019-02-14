math.randomseed(os.time())

local n = 100 -- <= 100
local maxC = 50 -- <= 50
local t = {}
local sumW = 0
for i = 1, n do
  local wi = math.random(1, 200)
  local vi = math.random(1, 10000)
  local ci = math.random(1, 50)
  table.insert(t, {wi, vi, ci})
  sumW = sumW + wi
end
maxW = math.min(10000, math.floor(sumW / 2))
local f = assert(io.open("dataset2.txt", "w"))
f:write(string.format("%d %d %d\n", n, maxW, maxC))
for i,v in ipairs(t) do
  f:write(string.format("%d %d %d\n", v[1], v[2], v[3]))
end
f:close()
