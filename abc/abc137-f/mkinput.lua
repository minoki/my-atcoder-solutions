local p = 1051 -- 2999
io.output(string.format("input%d.txt", p))
io.write(tostring(p), "\n")
local t = {}
for i = 0, p-1 do
  table.insert(t, "1")
end
io.write(table.concat(t, " "), "\n")
