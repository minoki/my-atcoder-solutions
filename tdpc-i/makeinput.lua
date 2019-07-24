local n = arg[1] and tonumber(arg[1]) or 5

local t = {}
for i = 1, n do
  local j = math.random(1, #t+1)
  table.insert(t, j, "i")
  table.insert(t, j, "w")
  table.insert(t, j, "i")
end
print(table.concat(t, ""))
