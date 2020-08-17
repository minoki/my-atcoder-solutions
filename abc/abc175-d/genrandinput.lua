local n = tonumber(arg[1]) or 1000
local k = tonumber(arg[2]) or 2324532

math.randomseed(os.time())

local P,C = {},{}
for i = 1,n do
  P[i] = i
  C[i] = math.random(-10^9, 10^9)
end

for i = 1,n-1 do
  local j = math.random(i+1,n)
  -- P[i],P[j] = P[j],P[i]
  P[i],P[i+1] = P[i+1],P[i]
end

io.write(string.format("%d %d\n", n, k))
io.write(table.concat(P," "),"\n")
io.write(table.concat(C," "),"\n")
