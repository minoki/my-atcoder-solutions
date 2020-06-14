local n = 2 * 10^5
local t = {}
table.insert(t, string.format("%d", 10^6))
for i = 2, n do
  t[i] = "1"
end
io.write(string.format("%d\n", n))
io.write(table.concat(t, " "), "\n")
